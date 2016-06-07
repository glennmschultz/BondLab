
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.




  options(digits = 8)
  utils::globalVariables(c("day", "month", "year"))


  days.in.month <- 30.44
  days.in.year <- 365.28
  days.in.year.360 <- 360
  weeks.in.year <- 52.25
  months.in.year <- 12
  trading.days <- 240
  min.principal <- 100
  zero.coupon <- 0
  pmt.frequency <- 2
  rate.delta <- .25
  ltv.basis <- 100
  price.basis <- 100
  yield.basis <- 100
  monthly.yield.basis <- 1200
  tolerance <- .000000001
  PSA.basis <- 100
  par.price <- 100

  #' @import methods
  NULL
