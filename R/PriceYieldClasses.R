

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.

  #' An S4 class representating bond price
  #' 
  #' This class is used to create and pass the price types reported to 
  #' investors and used in analytics.  For example price is often reported as
  #' decimal or fractions 32nds to investors but price basis (price/100) is
  #' used to calculate proceeds and compute metrics like yield, duration, and
  #' partial durations.
  #' @slot PriceDecimal A numeric value the price using decimal notation
  #' @slot Price32nds A character the price using 32nds notation
  #' @slot PriceBasis A numeric value price decimal notation in units of 100
  #' @exportClass PriceTypes
  setClass("PriceTypes",
           representation(
             PriceDecimal =  "numeric",
             Price32nds = "character",
             PriceBasis = "numeric")
           )

  setGeneric("PriceType", function(price)
    {standardGeneric("PriceType")})
  
  #' A standard generic function access the slot PriceDecimal
  #' 
  #' @param object an S4 object
  #' @export PriceDecimal
  setGeneric("PriceDecimal", function(object)
    {standardGeneric("PriceDecimal")})
  
  #' A standard generic function to replace the slot PriceDecimal
  #' 
  #' @param object an S4 object
  #' @param value the replacement value of the slot
  #' @export PriceDecimal<-
  setGeneric("PriceDecimal<-", function(object, value)
    {standardGeneric("PriceDecimal<-")})
  
  #' A standard generic function to access the slot Price32nds
  #' 
  #' @param object an S4 object
  #' @export Price32nds
  setGeneric("Price32nds", function(object)
    {standardGeneric("Price32nds")})
  
  #' A standard generic function to replace the slot Price32nds
  #' 
  #' @param object an S4 object
  #' @param value the replacement value of the slot
  #' @export Price32nds<-
  setGeneric("Price32nds<-", function(object, value)
    {setGeneric("Price32nds")})
  
  #' A standard generic to access the slot PriceBasis
  #' 
  #' @param object an S4 object
  #' @export PriceBasis
  setGeneric("PriceBasis", function(object)
    {standardGeneric("PriceBasis")})
  
  #' A standard generic to replace the slot PriceBasis
  #' 
  #' @param object A S4 object of type PriceTypes
  #' @param value the replacement value of the slot
  #' @export PriceBasis<-
  setGeneric("PriceBasis<-", function(object, value)
    {standardGeneric("PriceBasis<-")})
  
  setMethod("initialize",
            signature("PriceTypes"),
            function(.Object,
                     PriceDecimal = numeric(),
                     Price32nds = "character",
                     PriceBasis = numeric(),
                     ...){
              callNextMethod(.Object,
                             PriceDecimal = PriceDecimal,
                             Price32nds = Price32nds,
                             PriceBasis = PriceBasis,
                             ...)
            })
  #' A method to extract PriceDecimal from slot of class PriceTypes
  #' 
  #' @param object an S4 object of the type PriceTypes
  #' @exportMethod PriceDecimal
  setMethod("PriceDecimal", signature("PriceTypes"),
            function(object){object@PriceDecimal})
  
  #' A method to replace PriceDecimal in slot of class PriceTypes
  #' 
  #' @param object an S4 object of the typre PriceTypes
  #' @param value the replacement value of the slot
  #' @exportMethod PriceDecimal<-
  setReplaceMethod("PriceDecimal", signature("PriceTypes"),
                   function(object, value){
                     object@PriceDecimal <- value
                     return(object)
                   })
  
  #' A method to extract Price32nds from slot of class PriceTypes
  #' 
  #' @param object an S4 object of the type PriceTypes
  #' @exportMethod Price32nds
  setMethod("Price32nds", signature("PriceTypes"),
            function(object){object@Price32nds})
  
  #' A method to replace Price32nds in slot of class PriceTypes
  #' 
  #' @param object an S4 object of the type PriceTypes
  #' @param value the replacement value of the slot
  setReplaceMethod("Price32nds", signature("PriceTypes"),
                   function(object, value){
                     object@Price32nds <- value
                     return(object)
                   })
  
  #' A method to extract PriceBasis from slot of class PriceTypres
  #' 
  #' @param object an S4 object of the type PriceType
  #' @exportMethod PriceBasis
  setMethod("PriceBasis", signature("PriceTypes"),
            function(object){object@PriceBasis})
  
  #' a method to replace PriceBasis from slot of class PriceTypes
  #' 
  #' @param object an S4 object of the type PriceTypes
  #' @param value the replacement value of the slot
  #' @exportMethod PriceBasis<-
  setReplaceMethod("PriceBasis", signature("PriceTypes"),
                   function(object, value){
                     object@PriceBasis <- value
                     return(object)
                   })
  
  #' A constructor function for the class PriceTypes
  #' 
  #' @param Price the price in decimal notation (example 100.125)
  #' @export PriceTypes
  PriceTypes <- function(Price = numeric()){
    PriceBasis = 100
    Convertto32nds <- function(Price){
      tail32nds = round(x = (Price - signif(x = Price, digits = 0)) * 32,
                         digits = 2)
      Price = paste(as.character(signif(x=Price, digits = 0)),
                    "-",
                    as.character(tail32nds),
                    sep = "")
                    return(Price)
    }
    new("PriceTypes",
        PriceDecimal = Price,
        Price32nds = Convertto32nds(Price = Price),
        PriceBasis = Price/PriceBasis)
  }