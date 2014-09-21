REMICAnalytics <- function(bond.id = "character", 
                           trade.date = "character",
                           settlement.date = "character",
                           method = "character",
                           collateral.price = numeric(),
                           tranche.price = numeric(),
                           PrepaymentAssumption = "character",
                           ...,
                           begin.cpr = begin.cpr,
                           end.cpr = end.cpr,
                           seasoning.period = numeric(),
                           CPR = numeric()){
  
  #connect to rates data 
  # ---- connect to rates data folder
  rates.data <- Rates(trade.date = trade.date)
  
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  MortgageCashFlow <-  REMICCashFlow(bond.id = "BondLabSMBSIO", 
                                     trade.date = "01-10-2013",
                                     settlement.date = "01-17-2013",
                                     collateral.price = collateral.price,
                                     tranche.price = tranche.price,
                                     PrepaymentAssumption = "MODEL")
  
  #The fifth step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  MortgageTermStructure <- MtgTermStructure(bond.id = bond.id, 
                                            original.bal = original.bal, 
                                            Rate.Delta = Rate.Delta, 
                                            TermStructure = TermStructure, 
                                            settlement.date = settlement.date, 
                                            principal = original.bal *  bond.id@MBSFactor, 
                                            price = price, 
                                            cashflow = MortgageCashFlow)
  
}