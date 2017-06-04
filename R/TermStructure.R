
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

  #---------------------------------------------------
  # Term strucutre calls package termstrc 
  # and holds forward and spot rates as slots to class Term Structure
  #---------------------------------------------------

  #' @include MortgageCashFlow.R
  NULL
  
  #' An S4 class the term structure data needed to price bonds
  #' 
  #' @slot TradeDate A character string the trade date in the format
  #' mm/dd/YYYY
  #' @slot Period A numeric value the period index of the next cash flow
  #' @slot Date A numeric value the date of the next cash flow
  #' @slot SpotRate A numeric value the one-month spot rate
  #' @slot ForwardRate A numeric value the one-month forward rate
  #' @slot TwoYearFwd A numeric vlaue the two-year forward rate
  #' @slot TenYearFwd A numeric value the ten-year forward rate
  #' @exportClass TermStructure
  setClass("TermStructure",
           representation(
             TradeDate = "character",
             Period = "numeric",
             Date = "character",
             SpotRate = "numeric",
             ForwardRate = "numeric",
             TwoYearFwd = "numeric",
             TenYearFwd = "numeric"))

  
  #' A generic function to access the slot TradeDate
  #' @param object an S4 class object
  #' @export TradeDate
  setGeneric("TradeDate", function(object)
    {standardGeneric("TradeDate")})
  
  #' A generic function to replace the slot TradeDate
  #' @param object an S4 class object
  #' @param value the replacement value
  #' @export TradeDate<-
  setGeneric("TradeDate<-", function(object, value)
    {standardGeneric("TradeDate<-")})
  
  # Note: Period standard generic is found in MortgageCashFlow.R
  
  #' a generic function to replace the slot Period
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export Period<-
  setGeneric("Period<-", function(object, value)
    {standardGeneric("Period<-")})
  
  #' A generic function to access the slot Date in class TermStructure
  #' @param object an S4 class object
  #' @export ForwardDate
  setGeneric("ForwardDate", function(object)
    {standardGeneric("ForwardDate")})
  
  #' A generic function to replace the slot Date in class TermStructure
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export ForwardDate<-
  setGeneric("ForwardDate<-", function(object,value)
    {standardGeneric("ForwardDate<-")})

  #' A generic function to access the slot SpotRate in class TermStructure
  #' @param object an S4 class object
  #' @export SpotRate
  setGeneric("SpotRate", function(object)
    {standardGeneric("SpotRate")})
  
  #' A generic function to replace slot SpotRate
  #' @param object an S4 class object
  #' @param value the value of the replacement
  #' @export SpotRate<-
  setGeneric("SpotRate<-", function(object, value)
    {standardGeneric("SpotRate<-")})

  #' A generic function to access the slot ForwardRate
  #' @param object an S4 class object
  #' @export ForwardRate
  setGeneric("ForwardRate", function(object)
    {standardGeneric("ForwardRate")})
  
  #' A generic function to replace the ForwardRate in class TermStructure
  #' @param object an S4 class object
  #' @param value the value of the replacement
  #' @export ForwardRate<-
  setGeneric("ForwardRate<-", function(object, value)
  {standardGeneric("ForwardRate<-")})
  
  #' A generic function to access the slot TwoYearForward in class TermStructure
  #' @param object an S4 class object
  #' @export TwoYearForward
  setGeneric("TwoYearForward", function(object)
    {standardGeneric("TwoYearForward")})
  
  #' A generic function to replace the slot TwoYearForward in class 
  #' TermStructure
  #' @param object an S4 class object
  #' @param value the value of the replacement
  #' @export TwoYearForward<-
  setGeneric("TwoYearForward<-", function(object, value)
  {standardGeneric("TwoYearForward<-")})
  
  #' A generic function to access the slot TenYearForward in class TermStructure
  #' @param object an S4 class object
  #' @export TenYearForward
  setGeneric("TenYearForward", function(object)
    {standardGeneric("TenYearForward")})
  
  #' A generic function to replace the slot TenYearForward in class 
  #' TermStructure
  #' @param object an S4 class object
  #' @param value the value of the replacement
  #' @export TenYearForward<-
  setGeneric("TenYearForward<-", function(object, value)
    {standardGeneric("TenYearForward<-")})
  
  setMethod("initialize",
          signature("TermStructure"),
          function(.Object,
                   TradeDate = "character",
                   Period = "numeric",
                   Date = "character",
                   SpotRate = "numeric",
                   ForwardRate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric",
                   ...)
          {
            callNextMethod(.Object,
                           TradeDate = TradeDate,
                           Period = Period,
                           Date = Date,
                           SpotRate = SpotRate,
                           ForwardRate = ForwardRate,
                           TwoYearFwd = TwoYearFwd,
                           TenYearFwd = TenYearFwd,
            ...)
            
          })
  
  #' Method to extract TradeDate from the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @exportMethod TradeDate
  setMethod("TradeDate", signature("TermStructure"),
            function(object){object@TradeDate})
  
  #' Method to replace TradeDate in class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @param value the replacement value of the slot
  #' @exportMethod TradeDate<-
  setReplaceMethod("TradeDate", signature("TermStructure"),
            function(object, value){
              object@TradeDate <- value
              return(object)
            })
  
  #' Method to extract Period from S4 class TermStructure
  #' @param object the name of the S4 object TermStructure
  #' @exportMethod Period
  setMethod("Period", signature("TermStructure"),
            function(object){object@Period})
  
  #' Method to replace Period in S4 class TermStructure
  #' @param object the name of the S4 object of type TermStrucuture
  #' @param value the replacement value of the slot
  #' @exportMethod Period<-
  setReplaceMethod("Period", signature("TermStructure"),
                   function(object, value){
                     object@Period <- value
                     return(object)
                   })
  
  #' Method to extract Date from the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @exportMethod ForwardDate
  setMethod("ForwardDate", signature("TermStructure"),
            function(object){object@Date})
  
  #' Method to replace Date in the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @param value the replacement value of the slot
  #' @exportMethod ForwardDate<-
  setReplaceMethod("ForwardDate", signature("TermStructure"),
            function(object,value){
              object@Date <- value
              return(object)
              })

  #' Method to extract SpotRate from the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @exportMethod SpotRate
  setMethod("SpotRate", signature("TermStructure"),
            function(object){object@SpotRate})
  
  #' Method to replace SpotRate in the class TermStructure
  #' @param object the name of the object of the type TermStructure
  #' @param value the replacement value
  #' @exportMethod SpotRate<-
  setReplaceMethod("SpotRate", signature("TermStructure"),
                   function(object, value){
                     object@SpotRate <- value
                     return(object)
                     })
  
  #' Method to extract SpotRate from the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @exportMethod ForwardRate
  setMethod("ForwardRate", signature("TermStructure"),
            function(object){object@ForwardRate})
  
  #' Method to replace ForwardRate in the class TermStructure
  #' @param object the name of the object of the type TermStructure
  #' @param value the replacement value
  #' @exportMethod ForwardRate<-
  setReplaceMethod("ForwardRate", signature("TermStructure"),
                   function(object, value){
                     object@ForwardRate <- value
                     return(object)
                     })
  
  #' Method to extract the TwoYearForward from the class TermStructure
  #' @param object the name of the object of the type TermStructure
  #' @exportMethod TwoYearForward
  setMethod("TwoYearForward", signature("TermStructure"),
            function(object){object@TwoYearFwd})
  
  #' Method to replace the TwoYearForward in the class TermStructure
  #' @param object the name of the object of the Type TermStructure
  #' @param value the replacement value
  #' @exportMethod TwoYearForward<-
  setReplaceMethod("TwoYearForward", signature("TermStructure"),
                   function(object, value){
                     object@TwoYearFwd <- value
                     return(object)
                   })
  
  #' Method to extract the TenYearForward from the class TermStructure
  #' @param object the name of the object of the type TermStructure
  #' @exportMethod TenYearForward
  setMethod("TenYearForward", signature("TermStructure"),
            function(object){object@TenYearFwd})
  
  #' Method to replace the TenYearForward in the class TermStructure
  #' @param object the name of the object of the type TermStructure
  #' @param value the value of the replacement
  #' @exportMethod TenYearForward<-
  setReplaceMethod("TenYearForward", signature("TermStructure"),
            function(object, value){
              object@TenYearFwd <- value
              return(object)
            })

  #' The TermStructure constructor function it is a wrapper function 
  #' around the package termstrc
  #' 
  #' This is a wrapper function around the R package termstrc.  
  #' The function passes swap rate data
  #' cash flows the to termstrc and creates the TermStructure object 
  #' used by Bondlab.
  #' The function call rates data processes the yield curve and derives cashflow
  #' for the daily close swap curve. A Rates object must be called in the local
  #' environment for this function to work.
  #' @param rates.data A character string representing the data for which 
  #' the user would like to call the swap curve
  #' @param method A character string indicating the fitting method 
  #' ns = Nelson Siegel, dl = Diebold Lee, sv = Severson, 
  #' asv = adjusted Severson, cs = cubic spline(not yet implemented).
  #' For addiition details see the termstrc documentation.
  #' @examples
  #' \dontrun{
  #' TermStructure(rates.data = "01-10-2013", method = "ns")}
  #' @importFrom lubridate %m+%
  #' @importFrom lubridate years
  #' @importFrom lubridate day
  #' @importFrom lubridate month
  #' @importFrom termstrc estim_nss estim_cs spotrates forwardrates
  #' @export TermStructure
  TermStructure <- function(rates.data, method = "dl"){
  
  #function(trade.date = "character", method = "character")  
  #Error Trap User inputs to the function
  if(missing(rates.data)) stop("missing rates data object")  
  
  #Default to Dibold Li
  if(missing(method)) {method = "dl"}
  
  #Default to parametric
  if(method == "cs") stop("cubic spline not implemented")
  
  #Check that the user input a valid method
  CheckMethod <- c("ns", "dl", "sv", "asv", "cs")
  if(!method %in% CheckMethod) stop ("Invalid 'method' Value")
  
  # pass the yield curve to the function
  rates.data <- rates.data
  
  #set the column counter to make cashflows for termstrucutre
  ColCount <- as.numeric(ncol(rates.data))
  Mat.Years <- as.numeric(rates.data[2,2:ColCount])
  Coupon.Rate <- as.numeric(rates.data[1,2:ColCount])
  Issue.Date <- as.Date(rates.data[1,1])
  
  #initialize coupon bonds S3 class
  #This can be upgraded when bondlab has portfolio function
  ISIN <- vector()
  MATURITYDATE <- vector()
  ISSUEDATE <- vector()
  COUPONRATE <- vector()
  PRICE <- vector()
  ACCRUED <- vector()
  CFISIN <- vector()
  CF <- vector()
  DATE <- vector()
  CASHFLOWS  <- list(CFISIN,CF,DATE)
  names(CASHFLOWS) <- c("ISIN","CF","DATE")
  TODAY <- vector()
  data <- list()
  TSInput <- list()
  
  ### Assign Values to List Items #########
  data = NULL
  data$ISIN <- colnames(rates.data[2:ColCount])
  data$ISSUEDATE <- rep(as.Date(rates.data[1,1]),ColCount - 1)
  
  data$MATURITYDATE <-
    sapply(Mat.Years, function(Mat.Years = Mat.Years, 
                               Issue = Issue.Date) {
      Maturity = if(Mat.Years < 1) {
        Issue %m+% months(round(Mat.Years * months.in.year))
        } else {Issue %m+% years(as.numeric(Mat.Years))}
    return(as.character(Maturity))
    }) 
  
  
  data$COUPONRATE <- ifelse(Mat.Years < 1, 0, Coupon.Rate)                  
  
  data$PRICE <- ifelse(Mat.Years < 1, 
                       (1 + (Coupon.Rate/100))^(Mat.Years * -1) * 100,
                       100)
  
  data$ACCRUED <- rep(0, ColCount -1)
  
  for(j in 1:(ColCount-1)){
    Vector.Length <- as.numeric(
      round(difftime(data[[3]][j],
                     data[[2]][j],
                     units = c("weeks"))/weeks.in.year,5))
    
    Vector.Length <- ifelse(round(Vector.Length) < 1, 1 , 
                            round(Vector.Length * pmt.frequency))
    
    data$CASHFLOWS$ISIN <- append(data$CASHFLOWS$ISIN, 
                                  rep(data[[1]][j],Vector.Length))
    
    data$CASHFLOWS$CF <- append(data$CASHFLOWS$CF,
      as.numeric(c(rep((data[[4]][j]/100/pmt.frequency), Vector.Length-1) * 
                     min.principal, 
              (min.principal + (data$COUPONRATE[j]/100/pmt.frequency)* 
                 min.principal))))
    
    by.months = ifelse(data[[4]][j] == 0, 
                       round(difftime(data[[3]][j], 
                                      rates.data[1,1])/days.in.month), 6) 
  # this sets the month increment so that cashflows can handle discount bills
  
  data$CASHFLOWS$DATE <- append(data$CASHFLOW$DATE,
  seq(as.Date(rates.data[1,1]) %m+% months(as.numeric(by.months)), 
  as.Date(data[[3]][j]), 
  by = as.character(paste(by.months, "months", sep = " "))))
    
  } #The Loop Ends here and the list is made
  
  data$TODAY <- as.Date(rates.data[1,1])
  TSInput[[as.character(rates.data[1,1])]] <- c(data)
  
  #set term strucuture input (TSInput) to class couponbonds
  class(TSInput) <- "couponbonds"
  
  TS <- TSInput
  
  #Fit the term structure of interest rates
  
  if(method != "cs") {TSFit <- estim_nss(dataset = TSInput, 
                                        group = as.character(rates.data[1,1]), 
                                        matrange = "all", method = method)
  } else {TSFit <- estim_cs(bonddata = TSInput, 
                     group = as.character(rates.data[1,1]), 
                     matrange = "all", rse = TRUE)}
  
  #Return the coefficient vector to be passed in to the spot and 
  #forward rate functions
  #Maybe have the method choosen based on the one that gives the smallest RMSE
  
  Vector <- switch(method,
  ns = unname(TSFit$opt_result[[1]]$par[c("beta0", 
                                          "beta1", 
                                          "beta2", 
                                          "tau1")]),
  dl = unname(TSFit$opt_result[[1]]$par[c("beta0", 
                                          "beta1", 
                                          "beta2")]),
  sv = unname(TSFit$opt_result[[1]]$par[c("beta0", 
                                          "beta1", 
                                          "beta2", 
                                          "tau1", 
                                          "beta3", 
                                          "tau2")]),
  asv = unname(TSFit$opt_result[[1]]$par[c("beta0", 
                                           "beta1", 
                                           "beta2", 
                                           "tau1", 
                                           "beta3", 
                                           "tau2")])
  #cs = need to figure this out
  )
  
  
  
  #Calculate the spot rate curve and determine the forward rates needed to 
  period <- seq(from = 1, to = 600, by = 1)
  #Use the date from the cashflow file
  date <- seq(as.Date(rates.data[1,1]) %m+% months(1), 
              as.Date(data[[3]][j]), by="1 months")
  spot.rate.curve <- if(method != "dl"){
    spotrates(method = method, 
              beta = Vector, 
              m = seq(from = 1/12, to = 600/12, by = 1/12))
    } else {
      spotrates(method = method, 
                beta = Vector, 
                m = seq(from = 1/12, to = 600/12, by = 1/12), 
                lambda = TSFit$lambda)}
  forward.rate.curve <- if(method != "dl"){
    forwardrates(method = method, 
                 beta = Vector, 
                 m = seq(from = 1/12, to = 600/12, by = 1/12))
  } else {
    forwardrates(method = method, 
                 beta = Vector, 
                 m = seq(from = 1/12, to = 600/12, by = 1/12),
                 lambda = TSFit$lambda)
  }
  
  Two.Year.Fwd <- Forward.Rate(spot.rate.curve, FwdRate.Tenor = 24)[1:600]
  Ten.Year.Fwd <- Forward.Rate(spot.rate.curve, FwdRate.Tenor = 120)[1:600]

  new("TermStructure",
      TradeDate = as.character(rates.data[1,1]),
      Period = as.numeric(period),
      Date = as.character(date),
      SpotRate = spot.rate.curve,
      ForwardRate = forward.rate.curve,
      TwoYearFwd = Two.Year.Fwd,
      TenYearFwd = Ten.Year.Fwd
  )} 
