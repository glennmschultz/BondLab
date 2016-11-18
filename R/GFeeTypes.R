

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
  
  #' An S4 class representing GFee (guarantee fee) GFeeTypes 
  #' 
  #' This class is used to calculate and pass along the guarantee fee paid 
  #' by the borrower or turst to guarantee agency pooling the MBS.  For example,
  #' GFee is quoted as numeric decimal or decimal string but basis is used in 
  #' cashflow calcualtions to determine the amount paid to guarantor
  #' @slot GFeeDecimal a numeric value GFee expressed as numeric decimal
  #' @slot GFeeBasis a numeric value GFee expressed as basis in numeric decimal 
  #' form
  #' @slot GFeeDecimalString a character string the GFee expressed as numeric 
  #' decimal
  #' @exportClass GFeeTypes 
  setClass("GFeeTypes",
           representation(
             GFeeDecimal = "numeric",
             GFeeBasis = "numeric",
             GFeeDecimalString = "character"
           ))
  
  setGeneric("GFeeTypes", function(GFee = numeric())
    {standardGeneric("GFeeTypes")})
  
  #' A standard generic function to access the slot GFeeDecimal
  #' 
  #' @param object an S4 object of the type GFeeTypes
  #' @export GFeeDecimal
  setGeneric("GFeeDecimal", function(object)
    {standardGeneric("GFeeDecimal")})
  
  #' A standard generic function to replace the value of the slot GFeeDecimal
  #' 
  #' @param object an S4 object of the type GFeeTypes
  #' @param value the replacement value of the slot
  #' @export GFeeDecimal<-
  setGeneric("GFeeDecimal<-", function(object, value)
    {standardGeneric("GFeeDecimal<-")})
  
  #' A standard generic function to access the value of the slot GFeeBasis
  #' 
  #' @param object an S4 object of the type GFeeTypes
  #' @export GFeeBasis
  setGeneric("GFeeBasis", function(object)
    {standardGeneric("GFeeBasis")})
  
  #' A standard generic function to replace the value of the slot GFeeBasis
  #' 
  #' @param object an S4 object of the type GFeeTypes
  #' @param value the replacement value of the slot 
  #' @export GFeeBasis<-
  setGeneric("GFeeBasis<-", function(object, value)
    {standardGeneric("GFeeBasis<-")})
  
  #' A standard generic functon to access the slot GFeeDecimalString
  #' 
  #' @param object an object of the type GFeeTypes
  #' @export GFeeDecimalString
  setGeneric("GFeeDecimalString", function(object)
    {standardGeneric("GFeeDecimalString")})
  
  #' A standard generic function to replace the slot GFeeDecimalString
  #' 
  #' @param object an object of the type GFeeTypes
  #' @param value the replacement value of the slot
  #' @export GFeeDecimalString<-
  setGeneric("GFeeDecimalString<-", function(object, value)
    {standardGeneric("GFeeDecimalString<-")})
  
  setMethod("initialize",
            signature("GFeeTypes"),
            function(.Object,
              GFeeDecimal = "numeric",
              GFeeBasis = "numeric",
              GFeeDecimalString = "character",
              ...
            )
            {callNextMethod(.Object,
                            GFeeDecimal = GFeeDecimal,
                            GFeeBasis = GFeeBasis,
                            GFeeDecimalString = GFeeDecimalString,
                            ...)
              })
  
  #' A method to access the slot GFeeDecimal from the object GFeeTypes
  #' 
  #' @param object an S4 object of the type GFeeTypes
  #' @exportMethod GFeeDecimal
  setMethod("GFeeDecimal", signature("GFeeTypes"),
            function(object){object@GFeeDecimal})
  
  #' A method to replace the slot GFeeDecimal in the object GFeeTypes
  #' 
  #' @param object an object of the type GFeeTypes
  #' @param value the replacement value of the slot
  #' @exportMethod GFeeDecimal
  setReplaceMethod("GFeeDecimal", signature("GFeeTypes"),
                   function(object, value){
                     object@GFeeDecimal <- value
                   })
  
  #' A method to access the slot GFeeBasis in the object GFeeTypes
  #' 
  #' @param object an object of the type GFeeTypes
  #' @exportMethod GFeeBasis
  setMethod("GFeeBasis", signature("GFeeTypes"),
            function(object){object@GFeeBasis})
  
  #' A method to replace the value of the slot GFeeBasis in the object GFeeTypes
  #' 
  #' @param object an object of the type GFeeTypes
  #' @param value the replacement value of the slot
  #' @exportMethod GFeeBasis
  setReplaceMethod("GFeeBasis", signature("GFeeTypes"),
                   function(object, value){
                     object@GFeeBasis <- value})
  
  #' A method to access the slot GFeeDecimalString in the object GFeeTypes
  #' 
  #' @param object an object of the type GFeeTypes
  #' @exportMethod GFeeDecimalString
  setMethod("GFeeDecimalString", signature("GFeeTypes"),
            function(object){object@GFeeDecimalString})
  
  #' A method to replace the value of the slot GFeeDecimalString in the 
  #' object GFeeTypes
  #' 
  #' @param object an S4 object of the type GFeeTypes
  #' @param value the replacement value of the slot
  #' @exportMethod GFeeDecimalString
  setReplaceMethod("GFeeDecimalString", signature("GFeeTypes"),
                   function(object, value){
                     object@GFeeDecimalString <- value
                   })
  
  #' GFeeTypes is a constructor function for the class GFeeTypes
  #' 
  #' @param GFee a numeric value the GFee reported to the investor and reported
  #' as a numeric decimal
  #' @export GFeeTypes
  GFeeTypes <- function(GFee = numeric()){
    GFeeBasis = 100
    
    ConverttoBasis <- function(GFee = numeric(), GFeeBasis = numeric()){
      GFee = GFee/GFeeBasis
      return(GFee)
    }
    
    ConverttoString <- function(GFee = numeric()){
      GFee = sprintf("%.8f", GFee)
      return(GFee)
    }
    new("GFeeTypes",
        GFee = GFee,
        GFeeBasis = ConverttoBasis(GFee = GFee, GFeeBasis = GFeeBasis),
        GFeeDecimalString = ConverttoString(GFee = GFee))
  }