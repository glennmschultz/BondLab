

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
  #' @slot SpreadBasisDecimal a numeric value the spread expressed as numeric 
  #' decimal (bps)
  #' @slot SpreadBasisString a character the spread expressed as numeric decimal (bps)
  #' @slot SpreadDecimal a numeric value the spread expressed as numeric decimal
  #' @slot SpreadDecimalString a character the spread expressed as numeric decimal 
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
  
  #' A standard generic function to get the slot SpreadBasis
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @export SpreadBasisDecimal
  setGeneric("SpreadBasisDecimal", function(object)
    {standardGeneric("SpreadBasisDecimal")})
  
  #' A standard generic function to set the value of the slot SpreadBasis
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @export SpreadBasisDecimal<-
  setGeneric("SpreadBasisDecimal<-", function(object, value)
    {standardGeneric("SpreadBasisDecimal<-")})
  
  #' A standard generic function to get the slot SpreadBasisString
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @export SpreadBasisString
  setGeneric("SpreadBasisString", function(object)
    {standardGeneric("SpreadBasisString")})
  
  #' A standard generic function to set the value of the slot SpreadBasisString
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @export SpreadBasisString<-
  setGeneric("SpreadBasisString<-", function(object, value)
    {setGeneric("SpreadBasisString<-")})
  
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
  
  #' A method to get SpreadBasisDecimal from slot of class SpreadTypes
  #' 
  #' @param object an object of the type SpreadTypes
  #' @exportMethod SpreadBasisDecimal
  setMethod("SpreadBasisDecimal", signature("SpreadTypes"),
            function(object){object@SpreadBasisDecimal})
  
  #' A method to set the value of the slot SpreadBasisDecimal of the 
  #' class SpreadTypes
  #' 
  #' @param object an object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @exportMethod SpreadBasisDecimal<-
  setReplaceMethod("SpreadBasisDecimal", signature("SpreadTypes"),
                   function(object, value){
                     object@SpreadBasisDecimal <- value
                     return(object)
                   })
  
  #' A method to get the value of the slot SpreadBasisString from the object
  #' of class SpreadTypes
  #' 
  #' @param object an S4 object of the class SpreadTypes
  #' @exportMethod SpreadBasisString
  setMethod("SpreadBasisString", signature("SpreadTypes"),
            function(object){object@SpreadBasisDecimal})
  
  #' A method to set the value of the slot SpreadBasisString of the
  #' class SpreadTypes
  #' 
  #' @param object and object of the class SpreadTypes
  #' @param value the replacement value of the slot
  #' @exportMethod SpreadBasisString<-
  setReplaceMethod("SpreadBasisString", signature("SpreadTypes"),
                   function(object, value){
                     object@SpreadBasisString <- value
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