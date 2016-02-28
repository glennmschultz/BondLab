# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2016  Glenn M Schultz, CFA

  #' A function to determine bond payment date
  #' 
  #' The function creates a vector of payment date from the 
  #' last payment date to the end payment date.
  #' @param issue.date A character vector the bond issue date.
  #' @param start.date A character vector the interest accrual
  #' start date followign the issue date.
  #' @param settlement.date A character vector the settlement date.
  #' @param nextpayment.date A character vector the date of the 
  #' next payment due to the bond holder.
  #' @param end.date A character vector the final payment (maturity) date of the bond
  #' @param payment.frequency A numeric value the number of payments. 
  #' made to the bondholder.
  #' @param payment.delay A numeric value the payment delay of the bond 
  #' @importFrom lubridate year
  #' @importFrom lubridate month
  #' @export 
  PaymentDate <- function(issue.date = "character",
                          start.date = "character",
                          settlement.date = "character",
                          nextpayment.date = "character",
                          end.date = "character",
                          payment.frequency = numeric(),
                          payment.delay = numeric()){
    
    paymentdate.interval = months.in.year/payment.frequency
    
    issue.date = as.Date(issue.date, "%m-%d-%Y")
    start.date = as.Date(start.date, "%m-%d-%Y")
    settlement.date = as.Date(settlement.date, "%m-%d-%Y")
    nextpayment.date = as.Date(nextpayment.date, "%m-%d-%Y")
    end.date = as.Date(end.date, "%m-%d-%Y")

    as.Date(c(if(settlement.date == issue.date) 
    {seq(start.date, end.date, by = paste(paymentdate.interval, "months"))} 
    else 
    {seq(nextpayment.date, end.date, by = paste(paymentdate.interval, 
                                            "months"))}), "%m-%d-%Y") + payment.delay
    
  }