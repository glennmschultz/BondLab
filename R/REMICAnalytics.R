# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics"



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
  REMIC.Tranche <<- MBS(MBS.id = bond.id)
  
  # ---- connect to rates data folder
  rates.data <- Rates(trade.date = trade.date)
  
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  MortgageCashFlow <-  REMICCashFlow(bond.id = bond.id, 
                                     trade.date = "01-10-2013",
                                     settlement.date = "01-17-2013",
                                     collateral.price = collateral.price,
                                     tranche.price = tranche.price,
                                     PrepaymentAssumption = PrepaymentAssumption,
                                     ...,
                                     begin.cpr = begin.cpr,
                                     end.cpr = end.cpr,
                                     seasoning.period = seasoning.period,
                                     CPR = CPR)
  
  Rate.Delta = .25
  #The fifth step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  MortgageTermStructure <- REMICTermStructure(bond.id = bond.id, 
                                            original.bal = REMIC.Tranche@TrancheOrigBal, 
                                            Rate.Delta = Rate.Delta, 
                                            TermStructure = TermStructure, 
                                            settlement.date = settlement.date,
                                            trade.date = trade.date,
                                            collateral.price = collateral.price,
                                            tranche.price = tranche.price, 
                                            cashflow = MortgageCashFlow)
  
}