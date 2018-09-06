

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  
  #' A S4 class representing SpreadTypes (Spread the the Curve)
  #' 
  #' The is class is used to calculate SpreadTypes which are in turne 
  #' used as input to calculate price given a yield curve, 
  #' WAL or benchmark, and spread to curve.
  #' @slot SpreadBasis a character the spread expressed in basis points
  #' @slot SpreadDecimal a numeric value the spread expressed as numeric decimal
  #' @slot SpreadDecimalString a character the spread expressed as numeric decimal 
  #' @exportClass SpreadTypes
  setClass ("SpreadTypes",
            representation(
              SpreadBasis = "character",
              SpreadDecimal = "numeric",
              SpreadDecimalString = "character"
              ))

  
  #' A standard generic function to get the slot SpreadBasis
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @export SpreadBasis
  setGeneric("SpreadBasis", function(object)
    {standardGeneric("SpreadBasis")})
  
  #' A standard generic function to set the value of the slot SpreadBasis
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @export SpreadBasis<-
  setGeneric("SpreadBasis<-", function(object, value)
    {setGeneric("SpreadBasis<-")})
  
  #' A standard generic function to get the slot SpreadDecimal
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @export SpreadDecimal
  setGeneric("SpreadDecimal", function(object)
    {standardGeneric("SpreadDecimal")})
  
  #' A standard generic function to set the value of the slot SpreadDecimal
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @param value the replacement value of the slot 
  #' @export SpreadDecimal<-
  setGeneric("SpreadDecimal<-", function(object, value)
  {standardGeneric("SpreadDecimal<-")})
  
  #' A standard generic function to get the slot SpreadDecimalString
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @export SpreadDecimalString
  setGeneric("SpreadDecimalString", function(object)
    {standardGeneric("SpreadDecimalString")})
  
  #' A standard generic function to set the value of the slot SpreadDecimalString
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @export SpreadDecimalString<-
  setGeneric("SpreadDecimalString<-", function(object, value)
    {standardGeneric("SpreadDecimalString<-")})
  
  setMethod("initialize",
            signature("SpreadTypes"),
            function(.Object,
                     SpreadBasis = "character",
                     SpreadDecimal = numeric(),
                     SpreadDecimalString = "character",
                     ...
                     ){
              callNextMethod(.Object,
                             SpreadBasis = SpreadBasis,
                             SpreadDecimal = SpreadDecimal,
                             SpreadDecimalString = SpreadDecimalString)
            })
  
  
  #' A method to get the value of the slot SpreadBasisString from the object
  #' of class SpreadTypes
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @exportMethod SpreadBasis
  setMethod("SpreadBasis", signature("SpreadTypes"),
            function(object){object@SpreadBasis})
  
  #' A method to set the value of the slot SpreadBasisString of the
  #' class SpreadTypes
  #' 
  #' @param object and object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @exportMethod SpreadBasis<-
  setReplaceMethod("SpreadBasis", signature("SpreadTypes"),
                   function(object, value){
                     object@SpreadBasis <- value
                     return(object)
                   })
  
  #' A method to get the value of the slot SpreadDecimal from the class SpreadTypes
  #' 
  #' @param object an object of the class SpreadTypes
  #' @exportMethod SpreadDecimal
  setMethod("SpreadDecimal", signature("SpreadTypes"),
            function(object){object@SpreadDecimal})
  
  #' A method to set the value of the slot SpreadDecimalBasis of the 
  #' class SpreadTypes
  #' 
  #' @param object an object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @exportMethod SpreadDecimal<-
  setReplaceMethod("SpreadDecimal", signature("SpreadTypes"),
                   function(object, value){
                     object@SpreadDecimal <- value
                     return(object)
                   })
  
  #' A method to get the value of the slot SpreadDecimalString from the
  #' class SpreadTypes
  #' 
  #' @param object an object of the class SpreadTypes
  #' @exportMethod SpreadDecimalString
  setMethod("SpreadDecimalString", signature("SpreadTypes"),
            function(object){object@SpreadDecimalString})
  
  #' A method to set the value of the slot SpreadDecimalString of the 
  #' class SpreadTypes
  #' 
  #' @param object an object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @exportMethod SpreadDecimalString<-
  setReplaceMethod("SpreadDecimalString", signature("SpreadTypes"),
                   function(object, value){
                     object@SpreadDecimalString <- value
                     return(object)
                   })
  
  #' SpreadTypes is a constructor function for the class SpreadTypes
  #' 
  #' @param spread a character string the spread entered in basis points
  #' @export SpreadTypes
    SpreadTypes <- function(spread){
      spread.basis = 100
      
      ConverttoDecimal <- function(spread, basis){
        spread = as.numeric(spread)/basis
        return(spread)}
      
      ConverttoString <- function(spread,basis){
        spread = as.numeric(spread)/basis
        spread = sprintf("%.8f", spread)
        return(spread)}
      
      new("SpreadTypes",
          SpreadBasis = spread,
          SpreadDecimal = ConverttoDecimal(spread = spread, basis = spread.basis),
          SpreadDecimalString = ConverttoString(spread = spread, basis = spread.basis)
      )
  }