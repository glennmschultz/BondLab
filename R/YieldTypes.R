  
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
  
  setGeneric("YieldTypes", function(yield)
    {standardGeneric("YieldTypes")})
  
  #' A standard generic function to access the slot YieldDecimal
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @export YieldDecimal
  setGeneric("YieldDecimal", function(object)
    {standardGeneric("YieldDecimal")})
  
  #' A standard generic function to replace the value in the slot YieldDecimal
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @param value the replacement value of the slot
  #' @export YieldDecimal<-
  setGeneric("YieldDecimal<-", function(object, value)
  {standardGeneric("YieldDecimal<-")})
  
  #' A standard generic function to access the slot YieldBasis
  #' 
  #' @param object an S4 object of the type YieldBasis
  #' @export YieldBasis
  setGeneric("YieldBasis", function(object)
    {standardGeneric("YieldBasis")})
  
  #' A standard generic function replace the value in the slot YieldBasis
  #' 
  #' @param object an S4 object of the type YieldBasis
  #' @param value the replacement value of the slot
  #' @export YieldBasis<-
  setGeneric("YieldBasis<-", function(object, value)
    {standardGeneric("YieldBasis<-")})
  
  #' A standard generic function to access the slot YieldDecimalString
  #' 
  #' @param object an S4 object of the type YieldTypes
  #' @export YieldDecimalString
  setGeneric("YieldDecimalString", function(object)
    {standardGeneric("YieldDecimalString")})
  
  #' A standard generic function to replace the value in the slot YieldDecimalString
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