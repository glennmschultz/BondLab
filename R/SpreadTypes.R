

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
  
  #' A S4 class representing SpreadTypes (Spread the the Curve)
  #' 
  #' The is class is used to calculate SpreadTypes which are in turne 
  #' used as input to calculate price given a yield curve, 
  #' WAL or benchmark, and spread to curve.
  #' @slot SpreadBasis a numeric value the spread expressed as numeric decimal
  #' @slot SpreadDecimal a numeric value the spread expressed as numeric decimal
  #' @slot SpreadDecimalString a character value the spread expressed as 
  #' numeric decimal 
  #' @exportClass SpreadTypes
  setClass ("SpreadTypes",
            representation(
              SpreadBasisDecimal = "numeric",
              SpreadBasisString = "character",
              SpreadDecimal = "numeric",
              SpreadDecimalString = "character"
              ))
  setGeneric("SpreadTypes", function(spread = "character")
  {standardGeneric("SpreadTypes")})
  
  #' A standard generic function to access the slot SpreadBasis
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @export SpreadBasisDecimal
  setGeneric("SpreadBasisDecimal", function(object)
    {standardGeneric("SpreadBasisDecimal")})
  
  #' A standard generic function to replace the value of the slot SpreadBasis
  #' 
  #' @param object an S4 object of the typre SpreadTypes
  #' @param value the replacement value of the slot
  #' @export SpreadBasisDecimal<-
  setGeneric("SpreadBasisDecimal<-", function(object, value)
    {standardGeneric("SpreadBasisDecimal<-")})
  
  #' A standard generic function to access the slot SpreadBasisString
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @export SpreadBasisString
  setGeneric("SpreadBasisString", function(object)
    {standardGeneric("SpreadBasisString")})
  
  #' A standard generic functioin to replace the value of the slot SpreadBasisString
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @param value the replacement value of the slot
  #' @export SpreadBasisString<-
  setGeneric("SpreadBasisString<-", function(object, value)
    {setGeneric("SpreadBasisString<-")})
  
  #' A standard generic function to access the slot SpreadDecimal
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @export SpreadDecimal
  setGeneric("SpreadDecimal", function(object)
    {standardGeneric("SpreadDecimal")})
  
  #' A standard generic function to replace the value of the slot SpreadDecimal
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @param value the replacement value of the slot 
  #' @export SpreadDecimal<-
  setGeneric("SpreadDecimal<-", function(object, value)
  {standardGeneric("SpreadDecimal<-")})
  
  #' A standard generic function to access the slot SpreadDecimalString
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @export SpreadDecimalString
  setGeneric("SpreadDecimalString", function(object)
    {standardGeneric("SpreadDecimalString")})
  
  #' A standard generic function to replace the value of the slot SpreadDecimalString
  #' 
  #' @param object an S4 object of the type SpreadTypes
  #' @export SpreadDecimalString<-
  setGeneric("SpreadDecimalString<-", function(object, value)
    {standardGeneric("SpreadDecimalString<-")})
  
  setMethod("initialize",
            signature("SpreadTypes"),
            function(.Object,
                     SpreadBasisDecimal = numeric(),
                     SpreadBasisString = "character",
                     SpreadDecimal = numeric(),
                     SpreadDecimalString = "character",
                     ...
                     ){
              callNextMethod(.Object,
                             SpreadBasisDecimal = SpreadBasisDecimal,
                             SpreadBasisString = SpreadBasisString,
                             SpreadDecimal = SpreadDecimal,
                             SpreadDecimalString = SpreadDecimalString)
            })