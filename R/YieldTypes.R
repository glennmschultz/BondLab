  
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
  
  #' A S4 class representing yield to maturity
  #' 
  #' This class is used to create and pass YieldTypes reported by Bond Lab 
  #' analytics and reported to investors.  It does not calculate yield to maturity
  #' rather it holds the value in decimal number, basis, and decimal string form.
  #' @slot YieldDecimal a numeric value the yield to maturity in decimal number form
  #' @slot YieldBasis a numeric value the yield to maturity in basis form
  #' @slot YieldDecimalString a character string the yield to maturity
  #' @exportClass YieldTypes
  setClass("YieldTypes",
    representation(
      YieldDecimal = "numeric",
      YieldBasis = "numeric",
      YieldDecimalString = "character")
    )
  
  
  #' A standard generic function to get the slot YieldDecimal
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @export YieldDecimal
  setGeneric("YieldDecimal", function(object)
    {standardGeneric("YieldDecimal")})
  
  #' A standard generic function to set the value in the slot YieldDecimal
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @param value the replacement value of the slot
  #' @export YieldDecimal<-
  setGeneric("YieldDecimal<-", function(object, value)
  {standardGeneric("YieldDecimal<-")})
  
  #' A standard generic function to get the slot YieldBasis
  #' 
  #' @param object an S4 object of the type YieldBasis
  #' @export YieldBasis
  setGeneric("YieldBasis", function(object)
    {standardGeneric("YieldBasis")})
  
  #' A standard generic function set the value in the slot YieldBasis
  #' 
  #' @param object an S4 object of the type YieldBasis
  #' @param value the replacement value of the slot
  #' @export YieldBasis<-
  setGeneric("YieldBasis<-", function(object, value)
    {standardGeneric("YieldBasis<-")})
  
  #' A standard generic function to get the slot YieldDecimalString
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @export YieldDecimalString
  setGeneric("YieldDecimalString", function(object)
    {standardGeneric("YieldDecimalString")})
  
  #' A standard generic function to set the value in the slot YieldDecimalString
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @param value the replacement value of the slot
  #' @export YieldDecimalString<-
  setGeneric("YieldDecimalString<-", function(object, value)
    {standardGeneric("YieldDecimalString<-")})
  
  setMethod("initialize",
            signature("YieldTypes"),
            function(.Object,
                     YieldDecimal = numeric(),
                     YieldBasis = numeric(),
                     YieldDecimalString = "character",
                     ...){
              callNextMethod(.Object,
                             YieldDecimal = YieldDecimal,
                             YieldBasis = YieldBasis,
                             YieldDecimalString = YieldDecimalString,
                             ...)
              })
  
  #' A method to extract the slot YieldDecimal from the class YieldTypes
  #' 
  #' @param object an S4 class object of the type YieldTypes
  #' @exportMethod YieldDecimal
  setMethod("YieldDecimal", signature("YieldTypes"),
            function(object){object@YieldDecimal})
  
  #' A method to set the vlaue in the slot YieldDecimal 
  #' 
  #' @param object an S4 class of the type YieldTypes
  #' @param value the replacement value of the slot
  #' @exportMethod YieldDecimal
  setReplaceMethod("YieldDecimal", signature("YieldTypes"),
                   function(object, value){
                     object@YieldDecimal <- value
                     return(object)
                   })
  
  #' A method to extract the slot YieldBasis from the class YieldTypes
  #' 
  #' @param object an S4 object class of the type YieldTypes
  #' @exportMethod YieldBasis
  setMethod("YieldBasis", signature("YieldTypes"),
            function(object){object@YieldBasis})
  
  #' A method to set the value of the slot YieldBasis
  #' 
  #' @param object an S4 object class of the type YieldTypes
  #' @param value the replacement value of the slot
  setReplaceMethod("YieldBasis", signature("YieldTypes"),
            function(object, value){
              object@YieldBasis <- value
              return(object)
            })
  
  #' A method to extract the slot YieldDecimalString from the class YieldTypes
  #' 
  #' @param object an S4 class object of the type YieldTypes
  #' @exportMethod YieldDecimalString
  setMethod("YieldDecimalString",signature("YieldTypes"),
            function(object){object@YieldDecimalString})
  
  #' A method to set the slot YieldDecimalstring from the class YieldTypes
  #' 
  #' @param object an S4 class object of the type YieldTypes
  #' @param value the replacement value of the slot
  setReplaceMethod("YieldDecimalString", signature("YieldTypes"),
                   function(object, value){
                     object@YieldDecimalString <- value
                     return(object)
                   })
  
  #' YieldTypes is the constructor function for the class YieldTypes
  #' 
  #' @param yield a numeric value the yield expresssed in basis format
  #' (expample 5.0 is 0.5)
  #' @export YieldTypes
  YieldTypes <- function(yield = numeric()){
    YieldBasis = 100
    
    ConverttoBasis <- function(yield = numeric(), yieldbasis = numeric()){
      yield <- yield / yieldbasis
      return(yield)
    }
    
    ConverttoString <- function(yield = numeric()){
      yield <- yield
      yield <- sprintf("%.8f", yield)
      return(yield)
    }
    
    new("YieldTypes",
        YieldDecimal = yield,
        YieldBasis = ConverttoBasis(yield = yield, yieldbasis = YieldBasis),
        YieldDecimalString = ConverttoString(yield = yield)
    )
  }
  