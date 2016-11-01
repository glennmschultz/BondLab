
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

  #' An S4 class representing the bond coupon
  #' 
  #' This class is used to pass coupon types reported to investors and used in
  #' analytics.  For example coupon is often reported as a decimal number
  #' (e.g. 5.0%) but the coupon basis is used to calculate interest due to the 
  #' investor.  In BondLab coupon is a numeric value as opposed to price which is
  #' a character value.
  #' @slot CouponDecimal A numeric value the coupon expressed in decimal notation
  #' eg 5.50
  #' @slot CouponBasis A numeric value the coupon expressed in basis notation
  #' eg 0.055
  #' @exportClass CouponTypes
  setClass("CouponTypes",
           representation(
             CouponDecimal = "numeric",
             CouponBasis = "numeric",
             CouponDecimalString = "character")
           )
  
  setGeneric("CouponTypes", function(coupon)
    {standardGeneric("CouponTypes")})