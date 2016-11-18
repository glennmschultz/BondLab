

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

  #' An S4 class representing ServicingFee Types
  #' 
  #' This class is used create and pass SerivicingFee Types reported to the 
  #' investor and used in cashflow analytics.  For example, serivicing fee is 
  #' reported to the investor as numeric decimal or decimal string but basis is
  #' used to determine the servicing fee paid to servicer.
  #' @slot ServicingFeeDecimal a numeric value servicing fee expressed as numeric
  #' decimal.
  #' @slot ServicingFeeBasis a numeric value the servicing fee expressed in
  #' basis notation e.g. 0.0025
  #' @slot ServicingFeeDecimalString a character string the servicing fee
  #' expressed in numeric decimal notation
  #' @exportClass ServicingFeeTypes
  setClass("ServicingFeeTypes",
           representation(
             ServicingFeeDecimal = "numeric",
             ServicingFeeBasis = "numeric",
             ServicingFeeDecimalString = "character")
  )
  
  setGeneric("ServicingFeeTypes", function(ServicingFee = numeric())
    {standardGeneric("ServicingFeeTypes")})
  
  #' A standard generic function to access the slot ServicingFeeDecimal
  #' 
  #' @param object an S4 object of the type ServicingFeeDecimal
  #' @export ServicingFeeDecimal
  setGeneric("ServicingFeeDecimal", function(object)
    {standardGeneric("ServicingFeeDecimal")})
  
  #' A standard generic function to replace the value of the slot 
  #' SerivcingFeeDecimal
  #' 
  #' @param object an S4 object of the type SerivicingFeeTypes
  #' @param value the replacement value of the slot
  #' @export ServicingFeeDecimal<-
  setGeneric("ServicingFeeDecimal<-", function(object, value)
    {standardGeneric("ServicingFeeDecimal<-")})
  
  #' A standard generic function to access the value of the slot 
  #' ServicingFeeBasis
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @export ServicingFeeBasis
  setGeneric("ServicingFeeBasis",function(object)
    {standardGeneric("ServicingFeeBasis")})
  
  #' A standard generic function to replace the value of the slot 
  #' ServicingFeeBasis
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @param value the replacement value of the slot
  #' @export ServicingFeeBasis<-
  setGeneric("ServicingFeeBasis<-", function(object, value)
    {standardGeneric("ServicingFeeBasis<-")})
  
  #' A standard generic function to access the value of the slot
  #' ServicingFeeDecimalString
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @export ServicingFeeDecimalString
  setGeneric("ServicingFeeDecimalString", function(object)
    {standardGeneric("ServicingFeeDecimalString")})
  
  #' A standard generic function to replace the value of the slot 
  #' ServicingFeeDecimalString
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @param value the replacement value of the slot
  setGeneric("ServicingFeeDecimalString<-", function(object, value)
  {standardGeneric("ServicingFeeDecimalString<-")})
  
  setMethod("initialize",
            signature("ServicingFeeTypes"),
            function(.Object,
                     ServicingFeeDecimal = numeric(),
                     ServicingFeeBasis = numeric(),
                     ServicingFeeDecimalString = "character",
                     ...)
              {callNextMethod(.Object,
                              ServicingFeeDecimal = ServicingFeeDecimal,
                              ServicingFeeBasis = ServicingFeeBasis,
                              ServicingFeeDecimalString = ServicingFeeDecimalString,
                              ...)
              })
  #' A method to access the slot ServicingFeeDecimal from the object 
  #' ServicingFeeTypes
  #' 
  #' @param object an object of the type ServicingFeeTypes
  #' @exportMethod ServicingFeeDecimal
  setMethod("ServicingFeeDecimal", signature("ServicingFeeTypes"),
            function(object){object@ServicingFeeDecimal})

  #' A method to replace the slot ServicingFeeBasis in the object 
  #' ServicingFeeTypes
  #' @param object an S4 object of the type ServicingFeetypes
  #' @param value the replacement value of the slot
  #' @exportMethod ServicingFeeDecimal
  setReplaceMethod("ServicingFeeDecimal", signature("ServicingFeeTypes"),
                   function(object, value){
                     object@ServicingFeeDecimal <- value
                   })
  
  #' A method to access the slot ServicingFeeBasis from the object 
  #' ServicingFeeTypes
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @exportMethod ServicingFeeBasis
  setMethod("ServicingFeeBasis", signature("ServicingFeeTypes"),
            function(object){object@ServicingFeeBasis})
  
  #' A method to replace the slot ServicingFeeBasis in object ServicingFeeTypes
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @param value the replacement value of the slot
  #' @exportMethod ServicingFeeBasis
  setReplaceMethod("ServicingFeeBasis", signature("ServicingFeeTypes"),
            function(object, value){
              object@ServicingFeeBasis <- value
            })
  
  #' A method to access the slot ServicingFeeDecimalString from the object
  #' ServicingFeeTypes
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @exportMethod ServicingFeeDecimalString
  setMethod("ServicingFeeDecimalString", signature("ServicingFeeTypes"),
            function(object){object@ServicingFeeDecimalString})
  
  #' A method to replace the slot ServicingFeeDecimalString in the object
  #' ServicingFeeTypes
  #' 
  #' @param object an S4 object of the type ServicingFeeTypes
  #' @param value the replacement value of the slot
  setReplaceMethod("ServicingFeeDecimalString", signature("ServicingFeeTypes"),
                   function(object, value){
                     object@ServicingFeeDecimalString <- value
                   })
  #' ServicingFeeTypes is constructor function for the ServicingFeeTypes class
  #' 
  #' The ServicingFeeTypes class converts ServicingFee, a decimal numeric value,
  #' to ServicingFeeBasis a numeric value and ServicingFeeDecimalString a
  #' character string
  #' @param ServicingFee a numeric value the serivicing fee paid to the
  #' servicer of the loan from the trust
  #' @export ServicingFeeTypes
  ServicingFeeTypes <- function(ServicingFee = numeric()){
    ServicingBasis = 100
    
    ConvertoBasis <- function(ServicingFee = numeric(), 
                              ServicingBasis = numeric()){
      ServicingFee = ServicingFee / ServicingBasis
      return(ServicingFee)
    }
    
    ConverttoString <- function(ServicingFee = numeric()){
      ServicingFee = sprintf("%.8f", ServicingFee)
      return(ServicingFee)
    }
    
    new("ServicingFeeTypes",
        ServicingFeeDecimal = ServicingFee,
        ServicingFeeBasis = ConvertoBasis(ServicingFee = ServicingFee, 
                                          ServicingBasis = ServicingBasis),
        ServicingFeeDecimalString = ConverttoString(ServicingFee = ServicingFee)
        )
  }