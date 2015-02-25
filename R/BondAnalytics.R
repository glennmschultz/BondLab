# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


# --------------------------------
# Bond Analytics Functions - THESE ARE THE BOND LAB ENGINES !!!
# These functions are different from the above they use the functions together
# to analyze a bond or mortgage backed security using the above functions and construct the appropriate objects (classes)  
# --------------------------------

# This function analyzes a standard non callable bond and serves as the constructor function
# These are the engines  
# -----------------------------------
BondAnalytics <- function (bond.id = "character", principal = numeric(), price = numeric(), trade.date = "character", 
                           settlement.date = "character", method = method) 
{
  
  #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) method = "ns"
  
  #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
  Rate.Delta = 1
  
  # The first step is to read in the Bond Detail
  bond.id <- readRDS(paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""))
  #Call the desired curve from rates data folder
  trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data <- readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
  
  #The first step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  #The second step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
  #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
  BondCashFlow <- BondCashFlows(bond.id = bond.id, principal = principal, settlement.date = settlement.date, price = price)
  
  #The third step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  BondTermStructure <- BondTermStructure(bond.id = BondCashFlow, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
                                         principal = principal, price = price, cashflow = BondCashFlow)
  
  new("BondAnalytics", bond.id, BondCashFlow, BondTermStructure, TermStructure)
  
}


