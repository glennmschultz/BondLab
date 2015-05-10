  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


  options(digits = 8)
  utils::globalVariables(c("day", "month", "year"))

  # =======================================================================================
  #Bond Lab MBS Constants
  # =======================================================================================

  days.in.month <- 30.44
  days.in.year <- 365.28
  days.in.year.360 <- 360
  weeks.in.year <- 52.25
  months.in.year <- 12
  min.principal <- 100
  zero.coupon <- 0
  pmt.frequency <- 2
  rate.delta <- .25
  price.basis <- 100
  yield.basis <- 100

#' @import methods
NULL