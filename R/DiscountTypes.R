
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.


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
  
  #' A standard generic to get the slot DisocuntBasis
  #' 
  #' @param object an S4 class object of the type DiscountTypes
  #' @export DiscountBasis
  setGeneric("DiscountBasis", function(object)
    {standardGeneric("DiscountBasis")})
  
  #' A standard generic to set the slot DiscountBasis
  #' 
  #' @param object an S4 class of the type DiscountTypes
  #' @param value the replacement value of the slot
  #' @export DiscountBasis<-
  setGeneric("DiscountBasis<-", function(object, value)
    {standardGeneric("DiscountBasis<-")})
  
  #' A standard generic to get the slot DiscountDecimalString
  #' 
  #' @param object an S4 class of the type DiscountTypes
  #' @export DiscountDecimalString
  setGeneric("DiscountDecimalString", function(object)
    {standardGeneric("DiscountDecimalString")})
  
  #' A standard  generic to set the slot DiscountDecimalString
  #' 
  #' @param object an S4 class of the type DiscountTypes
  #' @param value the replacement value of the slot
  #' @export DiscountDecimalString<-
  setGeneric("DiscountDecimalString<-", function(object, value)
    {standardGeneric("DiscountDecimalString<-")})
  
  setMethod("initialize",
            signature("DiscountTypes"),
            function(.Object,
                     DiscountDecimal = numeric(),
                     DiscountBasis = numeric(),
                     DiscountDecimalString = "character",
                     ...)
              {callNextMethod(.Object,
                              DiscountDecimal = DiscountDecimal,
                              DiscountBasis = DiscountBasis,
                              DiscountDecimalString = DiscountDecimalString,
                              ...)
            })
  
  #' A method to extract the slot DiscountDecimal from the object DiscountTypes
  #' 
  #' @param object an S4 object of the type DiscountTypes
  #' @exportMethod DiscountDecimal
  setMethod("DiscountDecimal", signature("DiscountTypes"),
            function(object){object@DiscountDecimal})
  
  #' A method to set the slot DiscountDecimal in the object DiscountTypes
  #' 
  #' @param object an S4 object of the type DiscountTypes
  #' @param value the replacement value of the slot
  #' @exportMethod DiscountDecimal
  setReplaceMethod("DiscountDecimal", signature("DiscountTypes"),
                   function(object, value){
                     object@DiscountDecimal <- value
                     return(object)})
  
  #' A method to extract the slot DiscountBasis from the object DiscountTypes
  #' 
  #' @param object an S4 object of the type DiscountTypes
  #' @exportMethod DiscountBasis
  setMethod("DiscountBasis", signature("DiscountTypes"),
            function(object){object@DiscountDecimal})
  
  #' A method to set the slot DiscountBasis in the object DiscountTypes
  #' 
  #' @param object an S4 object of the type DiscountTypes
  #' @param value the replacement value of the slot
  #' @exportMethod DiscountBasis 
  setReplaceMethod("DiscountBasis", signature("DiscountTypes"),
                   function(object, value){
                     object@DiscountBasis <- value
                     return(object)})
  
  #' A method to extract the slot DiscountDecimalString from the object DiscountTypes
  #' 
  #' @param object an S4 object of the type DiscountTypes
  #' @exportMethod DiscountDecimalString
  setMethod("DiscountDecimalString", signature("DiscountTypes"),
            function(object){object@DiscountDecimalString})
  
  #' A method to set the slot DiscountDecimalString from the object DiscountTypes
  #' 
  #' @param object an object of the type DiscountTypes
  #' @param value  the replacement value of the slot
  #' @exportMethod DiscountDecimalString
  setReplaceMethod("DiscountDecimalString", signature("DiscountTypes"),
                   function(object, value){
                     object@DiscountDecimalString <- value
                     return(object)})
  
  #' DiscountTypes is the constructor function for the DiscountTypes class
  #' 
  #' The DiscountTypes class converts the discount rate from a numeric value to a
  #' basis value (divided by 100) and a string (basis value)
  #' @param discount.rate A numeric value the stated discount rate of the bill
  #' @export DiscountTypes
  DiscountTypes <- function(discount.rate = numeric()){
    DiscountBasis = 100
    
    ConverttoBasis <- function(discount.rate = numeric(), DiscountBasis = numeric()){
      discount.rate = discount.rate / DiscountBasis
      return(discount.rate)
    }
    
    ConverttoString <- function(discount.rate = numeric()){
      discount.rate = sprintf("%.8f", discount.rate)
      return(discount.rate)
    }
    
    new("DiscountTypes",
        DiscountDecimal = discount.rate,
        DiscountBasis = ConverttoBasis(discount.rate = discount.rate, DiscountBasis = DiscountBasis),
        DiscountDecimalString = ConverttoString(discount.rate = discount.rate)
    )
    
  }