  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics"

  #----------------------------
  #Bond basis function This function set the interest payment day count basis 
  #----------------------------
  # Note use switch to add additional basis default will be 30/360

  #' Functions to convert bond payments to their interest payment basis
  #' 
  #' Conversion for payment date (currently 30/360 is supported)
  #' @param issue.date A character value the issue date of the security
  #' @param start.date A character value the start date for interest payment (dated date)
  #' @param end.date A character value the last payment date
  #' @param settlement.date A character value the settlement date
  #' @param lastpmt.date  A character value the last payment date to the investor
  #' @param nextpmt.date A character value the next payment date to the investor
  #' @importFrom lubridate year
  #' @importFrom lubridate month
  #' @export
  BondBasisConversion <- function(issue.date = "character", 
                                  start.date = "character", 
                                  end.date = "character", 
                                  settlement.date = "character",
                                  lastpmt.date = "character", 
                                  nextpmt.date = "character"){
  
  #This function converts day count to bond U.S. Bond Basis 30/360 day count calculation 
  #It returns the number of payments that will be received, period, and n for discounting
  #issue.date is the issuance date of the bond
  #start.date is the dated date of the bond
  #end.date is the maturity date of the bond
  #settlement.date is the settlement date of hte bond
  #lastpmt.date is the last coupon payment date
  #nextpmt.date is the next coupon payment date
  
  d1 <- if(settlement.date == issue.date) {day(issue.date)} else {day(settlement.date)}    
  m1 <- if(settlement.date == issue.date) {month(issue.date)} else {month(settlement.date)}
  y1 <- if(settlement.date == issue.date) {year(issue.date)} else {year(settlement.date)}
  d2 <- day(nextpmt.date)
  m2 <- month(nextpmt.date)
  y2 <- year(nextpmt.date)
  
  (max(0, 30 - d1) + min(30, d2) + 360*(y2-y1) + 30*(m2-m1-1))/360}
  
  
  setGeneric("BondBasisConversion", function(issue.date, start.date, end.date, settlement.date, 
                                             lastpmt.date, nextpmt.date)
    {standardGeneric("BondBasisConversion")})
