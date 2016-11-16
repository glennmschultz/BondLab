
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
  #' This class is used to create and pass coupon types reported to investors 
  #' used in analytics.  For example, coupon is often reported as a decimal number
  #' (e.g. 5.0%) but the coupon basis is used to calculate interest due to the 
  #' investor.  In BondLab coupon is a numeric value in all slots as opposed to 
  #' price which is a character value in all slots.
  #' @slot CouponDecimal A numeric value the coupon expressed in decimal notation
  #' eg 5.50
  #' @slot CouponBasis A numeric value the coupon expressed in basis notation
  #' eg 0.055
  #' @slot CouponDecimalString A character value the coupon expressed as a string
  #' using decimal notation eg "5.50".
  #' @exportClass CouponTypes
  setClass("CouponTypes",
           representation(
             CouponDecimal = "numeric",
             CouponBasis = "numeric",
             CouponDecimalString = "character")
           )
  
  setGeneric("CouponTypes", function(coupon)
    {standardGeneric("CouponTypes")})
  
  #' A standard generic function to access the slot CouponDecimal
  #' 
  #' @param object an S4 class object
  #' @export CouponDecimal
  setGeneric("CouponDecimal", function(object)
    {standardGeneric("CouponDecimal")})
  
  #' A standard generic function to replace the slot CouponDecimal
  #' 
  #' @param object an S4 class object of the type CouponTypes
  #' @param value the replacement value of the slot
  #' @export CouponDecimal<-
  setGeneric("CouponDecimal<-", function(object, value)
    {setGeneric("CouponDecimal<-")})
  
  #' A standard generic function to access the slot CouponBasis
  #' 
  #' @param object an S4 class object
  #' @export CouponBasis
  setGeneric("CouponBasis", function(object)
    {standardGeneric("CouponBasis")})
  
  #' A standard generic function ot replace the slot CouponBasis
  #' 
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export CouponBasis<-
  setGeneric("CouponBasis<-", function(object, value)
    {standardGeneric("CouponBasis<-")})
  
  #' A standard generic function to access the slot CouponDecimalString
  #'
  #'@param object an S4 class object
  #'@export CouponDecimalString
  setGeneric("CouponDecimalString", function(object)
    {standardGeneric("CouponDecimalString")})
  
  #' A standard generic functon to replace the slot CouponDecimalString
  #' 
  #' 
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export CouponDecimalString<-
  setGeneric("CouponDecimalString<-", function(object,value)
    {standardGeneric("CouponDecimalString<-")}) 
  
  setMethod("initialize",
    signature("CouponTypes"),
    function(.Object,
             CouponDecimal = numeric(),
             CouponBasis = numeric(),
             CouponDecimalString = "character",
             ...)
      {callNextMethod(.Object,
                      CouponDecimal = CouponDecimal,
                      CouponBasis = CouponBasis,
                      CouponDecimalString = CouponDecimalString,
                      ...)
    })
  
  #' A method to extract CouponDecimal from object CouponTypes
  #' 
  #' @param object an S4 object of the type CouponTypes
  #' @exportMethod CouponDecimal
  setMethod("CouponDecimal", signature("CouponTypes"),
            function(object){object@CouponDecimal})
  
  #' A method to replace CouponDecimal in the slot of CouponTypes
  #' 
  #' @param object an S4 object of the type CouponTypes
  #' @param value the replacement value of the slot
  #' @exportMethod CouponDecimal
  setReplaceMethod("CouponDecimal", signature("CouponTypes"),
                   function(object,value){
                     object@CouponDecimal <- value
                     return(object)})
  
  #' A method to extract CouponBasis from the slot of CouponTypes
  #' 
  #' @param  object an S4 object of the type CouponTypes
  #' @exportMethod CouponBasis
  setMethod("CouponBasis", signature("CouponTypes"),
            function(object){object@CouponBasis})
  
  #' A method to replace CouponBasis in the slot of CouponTypes
  #'
  #'@param object an S4 object of the type CouponTypes
  #'@param value the replacement value of the slot
  #'@exportMethod CouponBasis
  setReplaceMethod("CouponBasis", signature("CouponTypes"),
                   function(object, value){
                     object@CouponBasis <- value
                     return(object)})
  
  #' A method to extract CouponDecimalString from the object CouponTypes
  #' 
  #' @param object an S4 object of the type CouponTypes
  #' @exportMethod CouponDecimalString
  setMethod("CouponDecimalString", signature("CouponTypes"),
            function(object){object@CouponDecimalString})
  
  #' A method to replace CouponDecimalString in the slot of CouponTypes
  #' 
  #' @param object an S4 object of the type CouponTypes
  #' @param value the replacement value of the slot
  #' @exportMethod CouponDecimalString
  setReplaceMethod("CouponDecimalString", signature("CouponTypes"),
                   function(object, value){
                     object@CouponDecimalString
                     return(object)
                   })
  
  #' CouponTypes is a constructor function for the CouponTypes class
  #' 
  #' The CouponTypes class converts coupon as a numeric value to a basis value
  #' (CouponBasis) used to calculate interest due and character string 
  #' CouponDecimalString
  #' @param coupon A numeric value the state coupon of the bond
  #' @export CouponTypes
  CouponTypes <- function(coupon = numeric()){
    CouponBasis = 100
    
    ConverttoBasis <- function(coupon = numeric(), CouponBasis = numeric()){
      coupon = coupon / CouponBasis
      return(coupon)
    }
    
    ConverttoString <- function(coupon = numeric()){
      coupon = sprintf("%.8f", coupon)
      return(coupon)
    }
    
    new("CouponTypes",
        CouponDecimal = coupon,
        CouponBasis = ConverttoBasis(coupon = coupon, CouponBasis = CouponBasis),
        CouponDecimalString = ConverttoString(coupon = coupon)
    )
  }  
  