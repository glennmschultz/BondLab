# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics"


  REMICAnalysis <- function(bond.id = "character", 
                            trade.date = "character",
                            settlement.date = "character",
                            collateral.price = numeric(),
                            tranche.price = numeric(),
                            PrepaymentAssumption = "character", 
                            ..., 
                            begin.cpr = numeric(), 
                            end.cpr = numeric(), 
                            seasoning.period = numeric(), 
                            CPR = numeric()){
    
  # Error Trap (?)
      
  # ---- connect to rates data folder
  rates.data <- Rates(trade.date = trade.date)

  # --- connect to mortgage rate class
  MortgageRate <- MtgRate()

  # --- call term structure model
  Termstructure <- TermStructure(rates.data = rates.data) 

  #-- call REMIC Trance
  REMIC.Tranche <- MBS(MBS.id = bond.id)

  #-- call REMIC Deal Date
  REMIC.Deal <- REMICDeal(remic.deal = REMIC.Tranche@DealName)
  
  issue.date <- as.Date(REMIC.Deal@DealPriceDate, "%m-%d-%Y") 
  start.date <- as.Date(REMIC.Tranche@TrancheDatedDate, "%m-%d-%Y")
  end.date <- as.Date(REMIC.Tranche@TrancheLastPmtDate, "%m-%d-%Y")
  settlement.date <- as.Date(c(settlement.date), "%m-%d-%Y")
  lastpmt.date <- as.Date(REMIC.Tranche@TrancheLastPmtDate, "%m-%d-%Y")
  nextpmt.date <- as.Date(REMIC.Tranche@TrancheNextPmtDate, "%m-%d-%Y")
    
  #  Validate the price passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(tranche.price <= 1) {tranche.price = tranche.price} else {tranche.price = tranche.price/100}

  Collateral <- REMICCollateral(bond.id = "BondLabSMBSIO", 
                  trade.date = trade.date,
                  settlement.date = settlement.date,
                  collateral.price = collateral.price,
                  PrepaymentAssumption = PrepaymentAssumption)

  REMIC.CashFlow <- do.call(source, list(file = "BondLabSMBS", local = TRUE))
  
  principal <- as.numeric(TrancheBeginValue[1,as.numeric(REMIC.Tranche@TrancheNumber),1])
  accrued.interest <- as.numeric(TrancheBeginValue[1, as.numeric(REMIC.Tranche@TrancheNumber),2])
   
  #solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  #it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value
  
  
  irr <- function(rate , 
                  time.period, 
                  cashflow, 
                  principal, 
                  price, 
                  accrued.interest){
                  pv = cashflow * (1/(1+rate) ^ time.period)
                  proceeds = (principal * price)
                  sum(pv) - (proceeds + accrued.interest)}
  

  
  ytm = uniroot(irr, 
                interval = c(lower = -.5, upper = 5),
                tol =.0000000001, 
                time.period = as.numeric(REMICCashFlow[,3]), 
                cashflow = as.numeric(REMICCashFlow[,6]), 
                principal = principal, 
                price = tranche.price, 
                accrued.interest = accrued.interest)$root
  
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency

return(Yield.To.Maturity)
#REMICCashFlow <<- REMICCashFlow
#return(sum(as.numeric(REMICCashFlow[,6])))

}