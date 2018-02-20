
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
  
  #' @title Discount To Price bill
  #' @family Pricing
  #' @description Returns the price of a discount bill given a discount rate
  #' and day count
  #' @param bill.id A character string the cusip number or bill.id
  #' @param discount A numeric value the discount rate stated as a percentage
  #' @param day.count A numeric value the day count used for pricing
  #' @param trade.date A character the trade date mm-dd-YYYY
  #' @param settlement.date A character the settlement date mm-dd-YYYY
  #' @export
  DiscountToPrice <- function(bill.id,
                              discount,
                              day.count,
                              trade.date,
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
  
  #' @title Price to Discount Yield bill
  #' @family Pricing
  #' @description Returns the discount yield of bill given price
  #' @param bill.id A character string the cusip number or bill.id
  #' @param price A character string the bill price
  #' @param day.count A numeric value the day count used for pricing
  #' @param trade.date A character the trade date mm-dd-YYYY
  #' @param settlement.date A character the settlement date mm-dd-YYYY
  #' @export
  PriceToDiscountYield <- function(bill.id,
                              price,
                              day.count,
                              trade.date,
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
  
  #' @title Price to Semi-Bond Yield bill
  #' @family Pricing
  #' @description Returns the semi-bond yield of bill given price
  #' @param bill.id A character string the cusip number or bill.id
  #' @param price A character string the bill price
  #' @param day.count A numeric value the day count used for pricing
  #' @param trade.date A character the trade date mm-dd-YYYY
  #' @param settlement.date A character the settlement date mm-dd-YYYY
  #' @export
  PriceToBondYield <- function(bill.id,
                               price,
                               day.count,
                               trade.date,
                               settlement.date){
    bond.yield = NULL
    price = PriceTypes(price = price)
    principal = 100
    settlement.date = as.Date(settlement.date, format = '%m-%d-%Y')
    maturity.date = as.Date(Maturity(bill.id), format = '%m-%d-%Y')
    days.to.maturity = as.numeric(difftime(maturity.date, settlement.date, units = 'days'))
    proceeds = PriceBasis(price) * price.basis
    bond.yield = ((principal - proceeds)/principal) * (day.count/days.to.maturity)
    bond.yield = bond.yield * yield.basis
    return(bond.yield)
  }