
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.
  
 
  options(digits = 8)
  utils::globalVariables(c("day", "month", "year"))


  days.in.month <- 30
  days.in.year <- 365.00
  days.in.leap.year <- 366.00
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
  semi.yield.basis <- 200
  tolerance <- .000000001
  PSA.basis <- 100
  par.price <- 100

  #' @import methods
  NULL
