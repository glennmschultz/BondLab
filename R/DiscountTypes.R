
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

  #' An S4 class representing the discount rate applied to bill and strips
  #' 
  #' This class is used to create and pass discount types reported to investors 
  #' used in analytics.  For example, discount is often reported as a decimal number
  #' (e.g. 5.0%) but the discount basis is used to calculate the price of the security
  #' In BondLab discount is a numeric value in all slots as opposed to 
  #' price which is a character value in all slots.
  #' @slot DiscountDecimal A numeric value the discount expressed in decimal 
  #' notation eg 1.05
  #' @slot DiscountBasis A numeric value discount expressed in basis notation
  #' eg 0.015
  #' @slot DiscountDecimalString A character value the discount rate expressed
  #' as a string expressed in decimal notation eg "5.50"
  #' @exportClass DiscountTypes
  setClass("DiscountTypes",
           representation(
             DiscountDecimal = "numeric",
             DiscountBasis = "numeric",
             DiscountDecimalString = "character")
           )
  #' A standard generic to get the slot DiscountDecimal
  #' 
  #' @param object an S4 class object of the type DiscountTypes
  #' @export DiscountDecimal
  setGeneric("DiscountDecimal", function(object)
    {standardGeneric("DiscountDecimal")})
  
  #' A standard generic to set the slot DiscountDecimal
  #' 
  #' @param object an S4 class of the type DiscountTypes
  #' @param value the replacement value of the slot
  #' @export DiscountDecimal<-
  setGeneric("DiscountDecimal<-", function(object, value)
    {standardGeneric("DiscountDecimal<-")})