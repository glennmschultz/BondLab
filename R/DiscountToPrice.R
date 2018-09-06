
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  
  #' @title Yield To Price bill
  #' @family Pricing
  #' @description Returns the price of a bill given a bond equivalent yield
  #' and day count
  #' @param bill.id A character string the cusip number or bill.id
  #' @param discount A numeric value the discount rate stated as a percentage
  #' @param day.count A numeric value the day count used for pricing
  #' @param settlement.date A character the settlement date mm-dd-YYYY
  #' @export
  BillYieldToPrice <- function(bill.id,
                              discount,
                              day.count,
                              settlement.date){
    price = NULL
    principal = 100
    discount = discount/yield.basis
    settlement.date = as.Date(settlement.date, format = '%m-%d-%Y')
    maturity.date = as.Date(Maturity(bill.id), format = '%m-%d-%Y')
    days.to.maturity = as.numeric(difftime(maturity.date, settlement.date, units = 'days'))
    price = principal * (1-(days.to.maturity * discount)/day.count)
    return(price)
  }
  
  #' @title Price to Yield bill
  #' @family Pricing
  #' @description Returns the bond equivalent yield of bill given price
  #' @param bill.id A character string the cusip number or bill.id
  #' @param price A character string the bill price
  #' @param day.count A numeric value the day count used for pricing
  #' @param settlement.date A character the settlement date mm-dd-YYYY
  #' @export
  BillPriceToYield <- function(bill.id,
                              price,
                              day.count,
                              settlement.date){
    discount.yield = NULL
    price = PriceTypes(price = price)
    principal = 100
    settlement.date = as.Date(settlement.date, format = '%m-%d-%Y')
    maturity.date = as.Date(Maturity(bill.id), format = '%m-%d-%Y')
    days.to.maturity = as.numeric(difftime(maturity.date, settlement.date, units = 'days'))
    proceeds = PriceBasis(price) * price.basis
    discount.yield = ((principal - proceeds)/principal) * (day.count/days.to.maturity)
    discount.yield = discount.yield * yield.basis
    return(discount.yield)
  }
  