# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#--------------------------
# Error trap function for bond inputs
#--------------------------  
ErrorTrap <- function(bond.id = "character", principal = numeric(), settlement.date = "character", price = numeric()) {
  #Error inputs by the user make sure all needed dates and values are passed to Bond Cash Flow
  issue.date = bond.id@IssueDate
  start.date = bond.id@DatedDate
  end.date = bond.id@Maturity
  lastpmt.date = bond.id@LastPmtDate
  nextpmt.date = bond.id@NextPmtDate
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  
  # Test Bond Inputs
  if(missing(issue.date)) stop("missing issue date")
  if(missing(start.date)) stop("missing start date")
  if(missing (end.date)) stop("missing end (Maturity) date")
  if(missing(lastpmt.date)) stop("missing last payment date")
  if(missing (nextpmt.date)) stop("missing next payment date")
  if(missing (coupon)) stop("missing coupon")
  if(missing (frequency)) stop("missing frequency")
  #Test Trade Inputs
  if (missing (settlement.date)) stop("missing settlement date")
  if(missing (principal)) stop("missing principal")
  if(missing (price)) stop("missing price")
  
  issue.date = as.Date(c(issue.date), "%m-%d-%Y")
  start.date = as.Date(c(start.date), "%m-%d-%Y")
  end.date = as.Date(c(end.date), "%m-%d-%Y")
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  lastpmt.date = as.Date(c(lastpmt.date), "%m-%d-%Y")
  nextpmt.date = as.Date(c(nextpmt.date), "%m-%d-%Y")
  
  #============= Pass dates into the function m-d-y ======================
  
  #Error Trapping on Dates settlement date < next payment date or settlement date , issue date    
  if(settlement.date > nextpmt.date) stop ("Settlement Date is Greater Than Next Payment Date")
  if(settlement.date < issue.date) stop ("Settlement Date is Less Than Issue Date")
  if(settlement.date < lastpmt.date) stop ("Settlment Date is Less Than Last Payment Date")
  #Error Trapping on frequency and payment dates
  
  #Error Trap the user's price input
  if(price <= 0) stop("No valid bond price")
  #Error Trap the user's coupon input
  if (coupon < 0 | coupon > 100) stop("No valid coupon specified.")
  #Note: Minimum demonination needs to be added to class bond information as well as function input
}
