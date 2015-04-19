# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics"

  #' The REMIC cash flow engine the constructor function for REMICCashFlow oject
  #' 
  #' The function is the constructor function for REMICCashFlow Object
  #' @param bond.id A character string the cusip of bond id
  #' @param trade.date A character string the trade date 
  #' @param settlement.date A character string the settlement date
  #' @param collateral.price A numeric value the price of the collateral
  #' @param tranche.price A numeric value the price of the tranche
  #' @param PrepaymentAssumption A character string the prepayment assumption
  #' @param ... Optional values when CPR or PPC input is used
  #' @param begin.cpr A numeric value the begin CPR of the PPC ramp
  #' @param end.cpr A numeric value the end CPR of the PPC ramp
  #' @param seasoning.period A numeric value the length of the PPC ramp seasoning period
  #' @param CPR A numeric value the CPR assumption used
  #' @param KeyRateTermStructure default value NULL used for REMIC analytics do not overrided
  #' @examples 
  #' \dontrun{
  #' REMICCashFlow(bond.id = "BondLabSMBSIO", trade.date = "01-10-2013", 
  #' settlement.date = "01-13-2013", method = "ns", collateral.price = 105.75, 
  #' tranche.price = as.numeric(price[i]), PrepaymentAssumption = "CPR", CPR = 6)}
  #' @export
  REMICCashFlow <- function(bond.id = "character", 
                            trade.date = "character",
                            settlement.date = "character",
                            collateral.price = numeric(),
                            tranche.price = numeric(),
                            PrepaymentAssumption = "character", 
                            ..., 
                            begin.cpr = numeric(), 
                            end.cpr = numeric(), 
                            seasoning.period = numeric(), 
                            CPR = numeric(),
                            KeyRateTermStructure = NULL){
    
  # Error Trap here for inputs using error trap code (?)
  # ---- connect to rates data folder
  rates.data <- Rates(trade.date = trade.date)  
  
  #-- call REMIC Tranche
  REMIC.Tranche <- MBS(MBS.id = bond.id)

  #-- call REMIC Deal Date
  REMIC.Deal <- REMICDeal(remic.deal = REMIC.Tranche@DealName)
  
  TermStructure <- if(is.null(KeyRateTermStructure)) {TermStructure(rates.data = rates.data)} else {KeyRateTermStructure}
  #TermStructure <- TermStructure(rates.data = rates.data)
  
  
  #-- Note in REMIC data TrancheLastPmtDate is the tranche legal final payment date
  #-- The last payment date is found in the REMIC Deal FactorData List
  
  lastpmt.date <- REMIC.Deal@FactorData[[as.numeric(REMIC.Tranche@TrancheNumber)]]@PaymentDate[length(REMIC.Deal@FactorData[[as.numeric(REMIC.Tranche@TrancheNumber)]]@PaymentDate)]
    
  issue.date <- as.Date(REMIC.Deal@DealPriceDate, "%m-%d-%Y") 
  start.date <- as.Date(REMIC.Tranche@TrancheDatedDate, "%m-%d-%Y")
  end.date <- as.Date(REMIC.Tranche@TrancheLastPmtDate, "%m-%d-%Y")
  settlement.date <- as.Date(c(settlement.date), "%m-%d-%Y")
  lastpmt.date <- as.Date(lastpmt.date, "%m-%d-%Y")
  nextpmt.date <- as.Date(REMIC.Tranche@TrancheNextPmtDate, "%m-%d-%Y")
    
  #  Validate the price passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  #  Has to be a better way to do this?
  if(tranche.price <= 2) {tranche.price = tranche.price} else {tranche.price = tranche.price/100}

  Collateral <- REMICCollateral(bond.id = bond.id, 
                  trade.date = trade.date,
                  settlement.date = settlement.date,
                  collateral.price = collateral.price,
                  PrepaymentAssumption = PrepaymentAssumption,
                  TermStructure = TermStructure,
                  ...,
                  begin.cpr = begin.cpr,
                  end.cpr = end.cpr,
                  seasoning.period = seasoning.period,
                  CPR = CPR,
                  KeyRateTermStructure = KeyRateTermStructure)

  REMIC.CashFlow <- do.call(source, 
                    list(file = REMICWaterFall(deal.name = as.character(REMIC.Tranche@DealName)), 
                    local = TRUE))
  
  principal <- as.numeric(TrancheBeginValue[1,as.numeric(REMIC.Tranche@TrancheNumber),1])
  accrued.interest <- as.numeric(TrancheBeginValue[1, as.numeric(REMIC.Tranche@TrancheNumber),2])
  principal.proceeds <- as.numeric(TrancheBeginValue[1,as.numeric(REMIC.Tranche@TrancheNumber),3])

  #set up present value array for calculations of duration, convexity, weighted average life
  rows <- length(REMIC.CashFlow$value[,1])
  col.names <- c("Present Value Factor", "Present Value", "Duration", "Convexity Time", "Cash Flow Convexity", "Convexity")
  PresentValueArray <- array(data = NA, c(rows, 6), dimnames = list(seq(c(1:rows)), col.names))
     
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
                interval = c(lower = -.75, upper = .75),
                tol =.0000000001,
                #extendInt = "yes",
                time.period = as.numeric(REMICCashFlow[,3]), 
                cashflow = as.numeric(REMICCashFlow[,6]), 
                principal = principal, 
                price = tranche.price, 
                accrued.interest = accrued.interest)$root
  
  Yield.To.Maturity <- (((1 + ytm)^(1/frequency))-1) * frequency
  
  #populate the present value array.  The present value array is used with the REMIC.CashFlow matrix to compute
  #Weighted average life, Modified Duration, and Convexity
  
  #PresentValueArray[,1] is the present value factors based on yield to maturity discounting
  PresentValueArray[,1] <-  1/((1+(Yield.To.Maturity/frequency))^(as.numeric(REMIC.CashFlow$value[,3]) * frequency))
  #PresentValueArray[,2] is the present value of the cash flows
  PresentValueArray[,2] <- as.numeric(PresentValueArray[,1]) * as.numeric(REMIC.CashFlow$value[,6])
  #PresentValueArray[,3] is the Duration Factors
  PresentValueArray[,3] <- as.numeric(as.numeric(REMIC.CashFlow$value[,3]) * 
                                          (as.numeric(PresentValueArray[,2])/(principal * tranche.price + accrued.interest)))
  #Convexity Factors
  PresentValueArray[,4] <- as.numeric(REMIC.CashFlow$value[,3]) * (1 + as.numeric(REMIC.CashFlow$value[,3]))
  
  PresentValueArray[,5] <- (as.numeric(REMIC.CashFlow$value[,6]) /
                            ((1 + ((Yield.To.Maturity)/frequency)) ^ ((as.numeric(REMIC.CashFlow$value[,3]) + 2) * frequency))) /
                                ((principal * tranche.price) + accrued.interest)
  
  PresentValueArray[,6] <- as.numeric(PresentValueArray[,4]) * as.numeric(PresentValueArray[,5])
  
  
  #Weighted Average Life - based on principal or interest depending on the trancheprincipal, notional = interest
  WAL = if(isTRUE(REMIC.Tranche@TranchePrincipalDesc %in% "NTL"))
    {sum((as.numeric(REMIC.CashFlow$value[,4]) * as.numeric(REMIC.CashFlow$value[,3]))/ sum((principal * tranche.price) + accrued.interest))} else
    {sum((as.numeric(REMIC.CashFlow$value[,5]) * as.numeric(REMIC.CashFlow$value[,3]))/ sum((principal * tranche.price) + accrued.interest))} 

   
  
  #Duration and Convexity
  Duration = apply(PresentValueArray, 2, sum)[3]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(PresentValueArray, 2, sum)[6] * .5
  
  
  new("REMICCashFlow",
  DealName = REMIC.Tranche@DealName,    
  TrancheName = REMIC.Tranche@TrancheName,
  TrancheNumber = REMIC.Tranche@TrancheNumber,
  Price = tranche.price,
  PrincipalProceeds = principal.proceeds,
  Accrued = accrued.interest,
  YieldToMaturity = Yield.To.Maturity,
  WAL = WAL,
  ModDuration = Modified.Duration,
  Convexity = Convexity,
  Period = as.numeric(REMIC.CashFlow$value[,1]),
  PmtDate = REMIC.CashFlow$value[,2],
  TimePeriod = as.numeric(REMIC.CashFlow$value[,3]),
  Interest = as.numeric(REMIC.CashFlow$value[,4]),
  Principal =   as.numeric(REMIC.CashFlow$value[,5]),
  TotalCashFlow = as.numeric(REMIC.CashFlow$value[,6]))
  
}
  
  setMethod("initialize",
            signature("REMICCashFlow"),
            function(.Object,
                     DealName = "character",
                     TrancheName = "character",
                     TrancheNumber = "character",
                     Price = numeric(),
                     PrincipalProceeds = numeric(),
                     Accrued = numeric(),
                     YieldToMaturity = numeric(),
                     WAL = numeric(),
                     ModDuration = numeric(),
                     Convexity = numeric(),
                     Period = numeric(),
                     PmtDate = "character",
                     TimePeriod = numeric(),
                     Interest = numeric(),
                     Principal = numeric(),
                     TotalCashFlow = numeric())
{
              .Object@DealName = DealName
              .Object@TrancheName = TrancheName
              .Object@TrancheNumber = TrancheNumber
              .Object@Price = Price
              .Object@PrincipalProceeds = PrincipalProceeds
              .Object@Accrued = Accrued
              .Object@YieldToMaturity = YieldToMaturity
              .Object@WAL = WAL
              .Object@ModDuration = ModDuration
              .Object@Convexity = Convexity
              .Object@Period = Period
              .Object@PmtDate = PmtDate
              .Object@TimePeriod = TimePeriod
              .Object@Interest = Interest
              .Object@Principal = Principal
              .Object@TotalCashFlow =TotalCashFlow
              
              return(.Object)
              callNextMethod(.Object,...)  
            })
  
  
  setGeneric("REMICCashFlow", function(bond.id = "character", 
                                       trade.date = "character",
                                       settlement.date = "character",
                                       collateral.price = numeric(),
                                       tranche.price = numeric(),
                                       PrepaymentAssumption = "character", 
                                       ..., 
                                       begin.cpr = numeric(), 
                                       end.cpr = numeric(), 
                                       seasoning.period = numeric(), 
                                       CPR = numeric(),
                                       KeyRateTermStructure = NULL)
                          {standardGeneric("REMICCashFlow")})  