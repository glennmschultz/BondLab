

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  
  #' An S4 class representing PMI (primary mortgage insurance) PMITypes
  #' 
  #' This class is used to calculate and pass PMITypes reported to the investor
  #' and used in cashflow analytics.  For example, PMI is reported to the investor
  #' as a numeric decimal or decimal string but basis is used to determime the PMI
  #' amount paid to the insurer by the homeowner.
  #' @slot PMIDecimal a numeric value the PMI fee expressed as decimal numeric
  #' @slot PMIBasis a numeric value the PMI free expresed as basis in decimal 
  #' numeric form
  #' @slot PMIDecimalString a character string the PMI expressed in decimal 
  #' form.
  #' @exportClass PMITypes
  setClass("PMITypes",
           representation(
             PMIDecimal = "numeric",
             PMIBasis = "numeric",
             PMIDecimalString = "character"
           ))

  
  #' A standard generic function to get the slot PMIDecimal
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @export PMIDecimal
  setGeneric("PMIDecimal", function(object)
  {standardGeneric("PMIDecimal")})
  
  #' A standard generic function to set the value of the slot PMIDecimal
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @param value the replacement value of the slot
  #' @export PMIDecimal<-
  setGeneric("PMIDecimal<-", function(object, value)
    {standardGeneric("PMIDecimal<-")})
  
  #' A standard generic function to acees the slot PMIBasis
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @export PMIBasis
  setGeneric("PMIBasis", function(object)
  {standardGeneric("PMIBasis")})
  
  #' A standard generic function to set the value of the slot PMIBasis
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @param value the replacement value of the slot
  #' @export PMIBasis<-
  setGeneric("PMIBasis<-", function(object, value)
    {standardGeneric("PMIBasis<-")})
  
  #' A standard generic function to get the slot PMIDecimalString
  #' 
  #' @param object an S4 object of the type PMIDecimalString
  #' @export PMIDecimalString
  setGeneric("PMIDecimalString", function(object)
    {setGeneric("PMIDecimalString")})
  
  #' A standard generic function to set the value of the slot PMIDecimalString
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @param value the replacement value of the slot
  #' @export PMIDecimalString<-
  setGeneric("PMIDecimalString<-", function(object, value)
    {setGeneric("PMIDecimalString<-")})
  
  setMethod("initialize",
            signature("PMITypes"),
            function(.Object,
                     PMIDecimal = numeric(),
                     PMIBasis = numeric(),
                     PMIDecimalString = "character",
                     ...)
              {callNextMethod(.Object,
                              PMIDecimal = PMIDecimal,
                              PMIBasis = PMIBasis,
                              PMIDecimalString = PMIDecimalString,
                              ...)
              })
  
  #' A method to get the slot PMIDecimal from the object PMITypes
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @exportMethod PMIDecimal
  setMethod("PMIDecimal", signature("PMITypes"),
            function(object){object@PMIDecimal})
  
  #' A method to set the value of the slot PMIBasis in the object PMITypes
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @param value the replacement value of the slot
  #' @exportMethod PMIDecimal
  setReplaceMethod("PMIDecimal", signature("PMITypes"),
                   function(object, value){
                     object@PMIDecimal <- value
                   })
  
  #' A method to get the slot PMIBasis from the object PMITypes
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @exportMethod PMIBasis
  setMethod("PMIBasis", signature("PMITypes"),
            function(object){object@PMIBasis})
  
  #' A method to set the value of the slot PMIBasis in the object PMITypes
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @param value the replacement value of the slot
  #' @exportMethod PMIBasis
  setReplaceMethod("PMIBasis", signature("PMITypes"),
                   function(object, value){
                     object@PMIBasis <- value
                   })
  
  #' A method to get the slot PMIDecimalString from the object PMITypes
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @exportMethod PMIDecimalString
  setMethod("PMIDecimalString", signature("PMITypes"),
            function(object){object@PMIDecimalString})
  
  #' A method to set the value of the slot PMIDecimalString in the object PMITypes
  #' 
  #' @param object an S4 object of the type PMITypes
  #' @param value the replacement value of the slot
  #' @exportMethod PMIDecimalString
  setReplaceMethod("PMIDecimalString", signature("PMITypes"),
                   function(object, value){
                     object@PMIDecimalString <- value
                   })
  #' PMITypes is a constructor function for the PMITypes class
  #' 
  #' @param PMI a numeric value the PMI paid to the insurer by the lender
  #' expressed in decimal numeric
  #' @export PMITypes
  PMITypes <- function(PMI = numeric()){
    PMIBasis = 100
    
    ConverttoBasis <- function(PMI = numeric(), PMIBasis = numeric()){
      PMI = PMI/PMIBasis
      return(PMI)
    }
    
    ConvertoString <- function(PMI = numeric()){
      PMI = sprintf("%.8f", PMI)
      return(PMI)
    }
    new("PMITypes",
        PMIDecimal = PMI,
        PMIBasis = ConverttoBasis(PMI = PMI, PMIBasis = PMIBasis),
        PMIDecimalString = ConvertoString(PMI = PMI))
  }