
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
  #' @slot PriceDecimalString A character the price using decimal notation
  #' @exportClass PriceTypes
  setClass("PriceTypes",
           representation(
             PriceDecimal =  "numeric",
             Price32nds = "character",
             PriceBasis = "numeric",
             PriceDecimalString = "character")
           )

  setGeneric("PriceTypes", function(price = numeric())
    {standardGeneric("PriceTypes")})
  
  #' A standard generic function get the slot PriceDecimal
  #' 
  #' @param object an S4 object
  #' @export PriceDecimal
  setGeneric("PriceDecimal", function(object)
    {standardGeneric("PriceDecimal")})
  
  #' A standard generic function to set the slot PriceDecimal
  #' 
  #' @param object an S4 object
  #' @param value the replacement value of the slot
  #' @export PriceDecimal<-
  setGeneric("PriceDecimal<-", function(object, value)
    {standardGeneric("PriceDecimal<-")})
  
  #' A standard generic function to get the slot Price32nds
  #' 
  #' @param object an S4 object
  #' @export Price32nds
  setGeneric("Price32nds", function(object)
    {standardGeneric("Price32nds")})
  
  #' A standard generic function to set the slot Price32nds
  #' 
  #' @param object an S4 object
  #' @param value the replacement value of the slot
  #' @export Price32nds<-
  setGeneric("Price32nds<-", function(object, value)
    {setGeneric("Price32nds")})
  
  #' A standard generic to get the slot PriceBasis
  #' 
  #' @param object an S4 object
  #' @export PriceBasis
  setGeneric("PriceBasis", function(object)
    {standardGeneric("PriceBasis")})
  
  #' A standard generic to set the slot PriceBasis
  #' 
  #' @param object A S4 object of type PriceTypes
  #' @param value the replacement value of the slot
  #' @export PriceBasis<-
  setGeneric("PriceBasis<-", function(object, value)
    {standardGeneric("PriceBasis<-")})
  
  #' A standard generic to get the slot PriceDecimalString
  #' 
  #' @param object A S4 object of the type PriceTypes
  #' @export
  setGeneric("PriceDecimalString", function(object)
    {setGeneric("PriceDecimalString")})
  
  #' A standard generic to set the slot PriceDecimalString
  #' 
  #' @param object A S4 object of the type PriceTypes
  #' @param value The replacement value of the slot
  #' @export
  setGeneric("PriceDecimalString<-", function(object, value)
    {setGeneric("PriceDecimalString<-")})
  
  setMethod("initialize",
            signature("PriceTypes"),
            function(.Object,
                     PriceDecimal = numeric(),
                     Price32nds = "character",
                     PriceBasis = numeric(),
                     PriceDecimalString = "character",
                     ...){
              callNextMethod(.Object,
                             PriceDecimal = PriceDecimal,
                             Price32nds = Price32nds,
                             PriceBasis = PriceBasis,
                             PriceDecimalString = PriceDecimalString,
                             ...)
            })
  #' A method to extract PriceDecimal from slot of class PriceTypes
  #' 
  #' @param object an S4 object of the type PriceTypes
  #' @exportMethod PriceDecimal
  setMethod("PriceDecimal", signature("PriceTypes"),
            function(object){object@PriceDecimal})
  
  #' A method to set PriceDecimal in slot of class PriceTypes
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
  
  #' A method to set Price32nds in slot of class PriceTypes
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
  
  #' a method to set PriceBasis from slot of class PriceTypes
  #' 
  #' @param object an S4 object of the type PriceTypes
  #' @param value the replacement value of the slot
  #' @exportMethod PriceBasis<-
  setReplaceMethod("PriceBasis", signature("PriceTypes"),
                   function(object, value){
                     object@PriceBasis <- value
                     return(object)
                   })
  
  #' A method to extract PriceDecimalString from slot of class PriceTypes
  #' 
  #' @param object an S4 object of type PriceTypes
  #' @exportMethod PriceDecimalString
  setMethod("PriceDecimalString", signature("PriceTypes"),
            function(object){object@PriceDecimalString})
  
  #' A method to set PriceDecimalString from slot of class PriceTypes
  #' 
  #' @param object an S4 object of type PriceTypes
  #' @param value the replacement value of the slot
  #' @exportMethod PriceDecimalString<-
  setReplaceMethod("PriceDecimalString", signature("PriceTypes"),
                   function(object, value){
                     object@PriceDecimalString <- value
                     return(object)})
  
  #' PriceTypes is a constructor function for the class PriceTypes
  #' 
  #' @param Price character the price in either 
  #' decimal notation (example "100.125") or 32nds notation (example "100-4")
  #' @export PriceTypes
  PriceTypes <- function(price = "character"){
    PriceBasis = 100
    Units = 32
    
    if(mode(price) != "character") stop ("price must be a character")
    if(is.na(strsplit(price, "\\.|\\-")[[1]][2]) == TRUE) stop (
      "tail value is missing")
    
    Convertto32nds <- function(Price = "character"){
      #convert price to numeric value
      Price = as.numeric(Price)
      tail32nds = round(x = (Price - floor(x = Price)) * 32, digits = 4)
      Price = paste(as.character(floor(x=Price)),
                    "-",
                    as.character(tail32nds),
                    sep = "")
      return(Price)
    }
      
      ConverttoDecimal <- function(Price = "character", Units = numeric()){
        SplitPrice = strsplit(as.character(Price), "-")
        handle = as.numeric(SplitPrice[[1]][1])
        TailDecimal = signif(as.numeric(SplitPrice[[1]][2])/Units,8)
        TailDecimal = gsub("(^|[^0-9])0+", "\\1", TailDecimal, perl = TRUE)
        Price = paste(as.character(handle),
                      as.character(TailDecimal),sep="")
        return(Price)
      }

    ConverttoString <- function(PriceDecimal = numeric()){
      sprintf("%.8f", PriceDecimal)
    }
      
      # Convert Price when entered as a decimal value
      if(grepl(".", as.character(price), fixed = TRUE) == TRUE){
        Price_Decimal = format(price, nsmall =2)
        Price_32nds = Convertto32nds(Price = price)
        Price_Basis = as.numeric(price) / PriceBasis
        Price_Decimal_String = ConverttoString(
          PriceDecimal = as.numeric(Price_Decimal))
      }
      
      if(grepl("-", as.character(price), fixed = TRUE) == TRUE){
        Price_Decimal = ConverttoDecimal(Price = price, Units = Units)
        Price_32nds = price
        Price_Basis = as.numeric(Price_Decimal)/PriceBasis
        Price_Decimal_String = ConverttoString(
          PriceDecimal = as.numeric(Price_Decimal))
      }

    new("PriceTypes",
        PriceDecimal = as.numeric(Price_Decimal),
        Price32nds = Price_32nds,
        PriceBasis = as.numeric(Price_Basis),
        PriceDecimalString = Price_Decimal_String
        )
  }