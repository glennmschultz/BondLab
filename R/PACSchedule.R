# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics"


setMethod("initialize",
          signature("REMICPACSchedule"),
          function(.Object,
                   DealName = "character",
                   Group = "numeric",
                   PmtDate = "character",
                   Balance = "numeric",
                   ScheduledPmt = "numeric"){
                  
                    .Object@DealName = DealName
                    .Object@Group = Group
                    .Object@PmtDate = PmtDate
                    .Object@Balance = Balance
                    .Object@ScheduledPmt = ScheduledPmt
                    })

REMICPACSchedule <- function(bond.id = "character",
                             original.bal = numeric(),
                             trade.date = "character",
                             settlement.date = "character",
                             price = numeric(),
                             begin.cpr = numeric(),
                             end.cpr = numeric(),
                             seasoning.period = numeric(),
                             lower.PSA = numeric(), 
                             upper.PSA = numeric()){
  
  if(missing(lower.PSA)) stop ("Missing Lower PSA")
  if(missing(upper.PSA)) stop ("Missing Upper PSA")
  
  if(lower.PSA < 10) stop ("Lower PSA must be in Percentage")
  if(upper.PSA < 10) stop ("Upper PSA must be in Percentage")
  
  # ---- connect to the bond data folder
  bond.id <- MBS(MBS.id = bond.id)
  # ---- connect to rates data folder
  
  rates.data <- Rates(trade.date = trade.date)

  # --- connect to mortgage rate model
   MortgageRate <- MtgRate()
  
  TermStructure <- TermStructure(rates.data = rates.data, method = "ns")
  Burnout <- bond.id@Burnout
  
  PSA.Band <- c(lower.PSA/100, upper.PSA/100)
  Principal <- list()
  
  for(i in 1 : 2){
    
      begin.cpr <- begin.cpr * PSA.Band[i]
      end.cpr <- end.cpr * PSA.Band[i]
  
    PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id,
                                                 TermStructure = TermStructure,
                                                 MortgageRate = MortgageRate,
                                                 Burnout = Burnout,
                                                 PrepaymentAssumption = "PPC",
                                                 begin.cpr = begin.cpr,
                                                 end.cpr = end.cpr,
                                                 seasoning.period = seasoning.period
                                                 )
    
    MortgageCashFlow <-  MortgageCashFlow(bond.id = bond.id,
                                     original.bal = original.bal,    
                                     settlement.date = settlement.date,
                                     price = price,
                                     PrepaymentAssumption = PrepaymentAssumption)
  
     Principal[[i]] <-MortgageCashFlow@ScheduledPrin + MortgageCashFlow@PrepaidPrin
  }
  Matrix = do.call(cbind,Principal)
  return(Matrix)
}