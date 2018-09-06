
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  
  #' An S4 class representing the borrower Gwac borrower (note rate) Types
  #' 
  #' This class is used to create and pass GWac (note rate) Types reported
  #' to the investor and used in the cash flow analytics.  For example, GWac
  #' is reported to investors as numeric decimal or numeric decimal string but
  #' the basis is used to determine the interest paid to the trust.
  #' @slot GWacDecimal A numeric value the GWac (borrower note rate)
  #' expressed as numeric decimal
  #' @slot GWacBasis A numeric value the GWac expressed in basis notation e.g.
  #' 0.055
  #' @slot GWacDecimalString A character string the GWac (borrower note rate)
  #' expressed as numeric decimal notation e.g. "5.50
  #' @exportClass GWacTypes
  setClass("GWacTypes",
           representation(
             GWacDecimal = "numeric",
             GWacBasis = "numeric",
             GWacDecimalString = "character")
           )
  
  
  #' A standard generic function to get the slot GWacDecimal
  #' 
  #' @param object an S4 class object of the type GWACTypes
  #' @export GWacDecimal
  setGeneric("GWacDecimal", function(object)
    {standardGeneric("GWacDecimal")})
  
  #' A standard generic function to set the value of slot GWacDecimal
  #' 
  #' @param object an S4 class object of the type GWacTypes  #' 
  #' @param value the replacement value of the slot
  #' @export GWacDecimal<-
  setGeneric("GWacDecimal<-", function(object, value)
    {standardGeneric("GWacDecimal<-")})
  
  #' A standard generic function to get the slot GWacBasis
  #' 
  #' @param object an S4 class object of the type GWacTypes
  #' @export GWacBasis
  setGeneric("GWacBasis", function(object)
    {standardGeneric("GWacBasis")})
  
  #' A standard generic function to set the slot GwacBasis
  #' 
  #' @param object an S4 class object of the type GWacTypes  #' 
  #' @param value the replacement value of the slot
  #' @export GWacBasis<-
  setGeneric("GWacBasis<-", function(object, value)
    {standardGeneric("GWacBasis<-")})
  
  #' A standard generic function to get the slot GWacDecimalString
  #' 
  #' @param object an S4 object of the type GWacTypes
  #' @export GWacDecimalString
  setGeneric("GWacDecimalString", function(object)
    {standardGeneric("GWacDecimalString")})
  
  #' A standard generic function to set the slot GWacDecimalString
  #' 
  #' @param object an S4 object of the type GWacDecimalBasis  #' 
  #' @param value the replacement value of the slot
  #' @export GWacDecimalString<-
  setGeneric("GWacDecimalString<-", function(object, value)
    {standardGeneric("GWacDecimalString<-")})
  
  setMethod("initialize",
            signature("GWacTypes"),
            function(.Object,
                     GWacDecimal = numeric(),
                     GWacBasis = numeric(),
                     GWacDecimalString = "character",
                     ...)
              {callNextMethod(.Object,
                              GWacDecimal = GWacDecimal,
                              GWacBasis = GWacBasis,
                              GWacDecimalString = GWacDecimalString,
                              ...)
            })
            
  #' A method to get GWacDecimal from the object GWacTypes
  #' 
  #' @param object an S4 object of the type GWacDecimal
  #' @exportMethod GWacDecimal
  setMethod("GWacDecimal", signature("GWacTypes"),
            function(object){object@GWacDecimal})
  
  #' A method to set the vlaue of GWacDecimal slot in the object GWacTypes
  #' 
  #' @param object an S4 object of the type GWacTypes
  #' @param value the replacement value of the slot
  #' @exportMethod GWacDecimal
  setReplaceMethod("GWacDecimal", signature("GWacTypes"),
                   function(object, value){
                     object@GWacDecimal <- value
                   })
  
  #' A method to get the value of GWacBasis slot in the object GWacTypes
  #' 
  #' @param object an S4 object of the tyoe GWacTypes
  #' @exportMethod GWacBasis
  setMethod("GWacBasis", signature("GWacTypes"),
            function(object){object@GWacBasis})
  
  #' A method to set the value of GWacBasis slot in the object GWacTypes
  #' 
  #' @param object an S4 object of the type GWacTypes
  #' @param value the replacement value of the slot
  #' @exportMethod GWacBasis
  setReplaceMethod("GWacBasis", signature("GWacTypes"),
                   function(object, value){
                     object@GWacBasis <- value
                   })
  
  #' A method to get the value of the slot GWacDecimalString slot in the
  #' object GWacTypes
  #' 
  #' @param object an S4 object of the type GwacTypes
  #' @exportMethod GWacDecimalString
  setMethod("GWacDecimalString", signature("GWacTypes"),
            function(object){object@GWacDecimalString}) 
  
  #' A method to set the value of GWacDecimalString slot in the 
  #' object GWacTypes
  #' 
  #' @param object an S4 object of the type GWacTypes
  #' @param value the replacement value of the slot
  #' @exportMethod GWacDecimalString
  setReplaceMethod("GWacDecimalString", signature("GWacTypes"),
                   function(object, value){
                     object@GWacTypes <- value
                   })
  #' GWacTypes is a constructor function for the GwacTypes class
  #' 
  #' The GWacTypes class converts Gross Wac a decimal numeric value to a 
  #' GWacBasis a numeric value and GWacDecimalString a character string
  #' @param GWac a numeric value the Gross WAC reported to the investor.  This 
  #' may also be the note rate in the case of a loan
  #' @export GWacTypes
  GWacTypes <- function(GWac = numeric()){
    GWacBasis = 100
    
    ConverttoBasis <- function(GWac = numeric(), GWacBasis = numeric()){
      GWac = GWac/GWacBasis
      return(GWac)}
    
    ConverttoString <- function(GWac = numeric()){
      GWac = sprintf("%.8f", GWac)
      return(GWac)}
    
    new("GWacTypes", 
        GWacDecimal = GWac,
        GWacBasis = ConverttoBasis(GWac = GWac, GWacBasis = GWacBasis),
        GWacDecimalString = ConverttoString(GWac = GWac))
  }
