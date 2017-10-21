
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
  
  
  #' A function to calculate forward rates
  #' 
  #' Calculate forward rate given a vector of spot rates
  #' @param term.structure an S4 object of the type TermStructure
  #' @param forward.tenor the forward tenor stated in months.  For example the
  #' two-year forward rate is specified as 24.
  #' @param type the calculation type "P" periodic or "C" continuous
  #' @importFrom stats predict
  #' @importFrom stats na.omit
  #' @importFrom splines interpSpline
  #' @export Forward.Rate
  Forward.Rate <- function(term.structure,
                           forward.tenor,
                           type = "C"){
    
    disc.rate = DiscRate(term.structure)
    spot.rate = SpotRate(term.structure)/yield.basis
    num.period = TimePeriod(term.structure)[1:length(spot.rate)]
    FutureValueVector <- (1 + spot.rate) ^ num.period
    max.maturity <- length(spot.rate)
    
    forward.rate <- 
    switch(type,
           C = -((log(disc.rate[(forward.tenor + 1) : length(disc.rate)]) -
                      log(disc.rate[1:(length(disc.rate) - forward.tenor)])) / 
                 (forward.tenor/months.in.year)),
           P = (FutureValueVector[(forward.tenor + 1):length(spot.rate)] / 
                  FutureValueVector[1 : (length(spot.rate) - (forward.tenor))]) ^ 
             (1/(forward.tenor/months.in.year))-1)

    forward.rate <- predict(
      splines::interpSpline(
        seq(1:length(forward.rate)), 
        forward.rate, 
        bSpline = TRUE, 
        na.action = na.omit)
      ,seq(1:length(forward.rate)))$y
    
    return(forward.rate)
    }
  
  #' An S4 class the term structure data needed to price bonds
  #' 
  #' @slot TradeDate A character string the trade date in the format
  #' mm/dd/YYYY
  #' @slot Period A numeric value the period index of the next cash flow
  #' @slot Date A numeric value the date of the next cash flow
  #' @slot TimePeriod a numeric value the time period between payments made
  #' to the investor
  #' @slot SpotRate A numeric value the one-month spot rate
  #' @slot ForwardRate A numeric value the one-month forward rate
  #' @slot DiscRate A numeric value the discount rate curve
  #' @slot TwoYearFwd A numeric vlaue the two-year forward rate
  #' @slot TenYearFwd A numeric value the ten-year forward rate
  #' @exportClass TermStructure
  setClass("TermStructure",
           representation(
             TradeDate = "character",
             Period = "numeric",
             Date = "character",
             TimePeriod = 'numeric',
             SpotRate = "numeric",
             ForwardRate = "numeric",
             DiscRate = "numeric",
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
  # Note: TimePeriod standard generic is found in MortgageCashFlow.R
  
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
  
  #' A generic function to access the slot DiscRate
  #' @param object an S4 class object
  #' @export DiscRate
  setGeneric("DiscRate", function(object)
    {standardGeneric("DiscRate")})
  
  #' A generic function to replace the slot DiscRate
  #' @param object an S4 class object
  #' @param value the value of the replacement
  #' @export DiscRate<-
  setGeneric("DiscRate<-", function(object, value)
    {standardGeneric("DiscRate<-")})
  
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
                   TimePeriod = 'numeric',
                   SpotRate = "numeric",
                   ForwardRate = "numeric",
                   DiscRate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric",
                   ...)
          {
            callNextMethod(.Object,
                           TradeDate = TradeDate,
                           Period = Period,
                           Date = Date,
                           TimePeriod = TimePeriod,
                           SpotRate = SpotRate,
                           ForwardRate = ForwardRate,
                           DiscRate = DiscRate,
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
  
  #' Method to get TimePeriod in the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @exportMethod TimePeriod
  setMethod("TimePeriod", signature("TermStructure"),
            function(object){object@TimePeriod})
  
  #' Method to replace TimePeriod in the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @param value the replacement value of the slot
  #' @exportMethod TimePeriod<-
  setReplaceMethod("TimePeriod", signature("TermStructure"),
                   function(object,value){
                     object@TimePeriod <- value
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
  
  #' Method to extract ForwardRate from the class TermStructure
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
  
  #' Method to extract DiscRate from the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @exportMethod DiscRate
  setMethod("DiscRate", signature("TermStructure"),
            function(object){object@DiscRate})
  
  #' Method to replace DiscRate from the class TermStructure
  #' @param object the name of the object of type TermStructure
  #' @param value the replacement value
  #' @exportMethod DiscRate<-
  setReplaceMethod("DiscRate", signature("TermStructure"),
                   function(object, value){
                     object@DiscRate <-value
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
  #' @importFrom bizdays add.bizdays
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
  
  # function to compute the price of T-Bill.  This is used to price
  # short discount securities for the Termstructure fitting
  BillPrice <- function(face = 100, disc.rate, days.mat){
    price = NULL
    price = face*(1-(disc.rate * days.mat)/360)
    return(price)}
  
  # create NULL CurveData object
  CurveData <- NULL
  # set calendar object for calculation of proper settlement date
  cal <- 'actual'
  
  # pass the yield curve to the function
  rates.data <- rates.data
  
  # Dimension UST rates data array
  USTData <- data.frame(matrix(data = NA, nrow = 2, ncol = 12))
  colnames(USTData) <- c('Date', 
                         'Bill1M', 
                         'Bill3M', 
                         'Bill6M', 
                         'Bill1', 
                         'Note2', 
                         'Note3', 
                         'Note5', 
                         'Note7', 
                         'Note10', 
                         'Note20', 
                         'Bond30')
  
  USTData[1,1] <- as.character(as.Date(
    as.character(rates.data[1,1]), format = '%Y-%m-%d'))
  USTData[1,2] <- rates.data[1,2] #1 month bill rate
  USTData[1,3] <- rates.data[1,3] #3 month bill rate
  USTData[1,4] <- rates.data[1,4] #6 month bill rate
  USTData[1,5] <- rates.data[1,5] #12 month libor rate 
  USTData[1,6] <- rates.data[1,6]  #2 year note rate
  USTData[1,7] <- rates.data[1,7]  #3 year note rate
  USTData[1,8] <- rates.data[1,8]  #5 year note rate
  USTData[1,9] <- rates.data[1,9]  #7 year note rate
  USTData[1,10] <- rates.data[1,10] #10 year note rate
  USTData[1,11] <- rates.data[1,11] #20 year bond rate
  USTData[1,12] <- rates.data[1,12] #30 year bond rate
  USTData[2,2] <- .0833
  USTData[2,3] <- .25
  USTData[2,4] <- .50
  USTData[2,5] <- 1
  USTData[2,6] <- 2
  USTData[2,7] <- 3
  USTData[2,8] <- 5
  USTData[2,9] <- 7
  USTData[2,10] <- 10
  USTData[2,11] <- 20
  USTData[2,12] <- 30
  
  treasurybonds <- list(ISIN = NULL,
                        MATURITYDATE = NULL,
                        ISSUDATE = NULL,
                        COUPONRATE = NULL,
                        PRICE = NULL,
                        ACCRUED = NULL,
                        CASHFLOWS = list(ISIN = NULL, CF = NULL, DATE = NULL),
                        TODAY = NULL)
  
  for(bonddata in 2:(length(USTData))){
    num.months = ceiling(USTData[2,bonddata] * 12)
    MaturityDate = as.character(as.Date(USTData[1,1]) %m+% months(num.months), format = '%m-%d-%Y')
    SettlementDate = as.character(add.bizdays(USTData[1,1], 2, cal), format = '%m-%d-%Y')
    IssueDate =as.character(as.Date(USTData[1,1]), format = '%m-%d-%Y')
    DatedDate = as.character(as.Date(USTData[1,1]), format = '%m-%d-%Y')
    Coupon = if(USTData[2,bonddata] <= 1){0.00} else {USTData[1,bonddata]}
    Price = if(USTData[2,bonddata] <= 1) {BillPrice(
      100, USTData[1,bonddata]/100, 
      floor(USTData[2,bonddata] * 364))} else {100}
    
    if(USTData[2,bonddata] <= 1){PmtDates = c(as.Date(SettlementDate, format = '%m-%d-%Y'),
                                              as.Date(MaturityDate, format = '%m-%d-%Y'))} else {
                                                PmtDates = LastandNextPmtDate(
                                                  issue.date = IssueDate,
                                                  dated.date = DatedDate,
                                                  maturity.date = MaturityDate,
                                                  settlement.date = SettlementDate,
                                                  bond.basis = 'Actual365',
                                                  frequency = 2)}
    
    ISIN = gsub("[[:punct:]]", "", paste0('UST',Coupon, 
                                          format(as.Date(MaturityDate, 
                                                         format = '%m-%d-%Y'), 
                                                 format ='%m%-%Y')))
    
    Bond <- BondDetails(
      Cusip = ISIN,
      ID = colnames(USTData[1]),
      BondType = if(USTData[1,1] <=1) {'Bill'} else {'Bond'},
      Sector = 'UST',
      Issuer = 'US Govt',
      Underwriter = 'US Govt',
      OfferAmount = 100,
      Coupon = Coupon,
      IssueDate = IssueDate,
      DatedDate = DatedDate,
      Maturity = MaturityDate,
      LastPmtDate = as.character(as.Date(PmtDates[1]), format = '%m-%d-%Y'),
      NextPmtDate = as.character(as.Date(PmtDates[2]), format = '%m-%d-%Y'),
      Moody = 'Aaa',
      SP = 'AAA',
      BondLab = 'AAA',
      Frequency = 2,
      BondBasis = 'Actual365',
      Callable = FALSE,
      Putable = FALSE,
      SinkingFund = FALSE
    )
    
    CashFlow <- CashFlowBond(bond.id = Bond, 
                             principal = OfferAmount(Bond), 
                             settlement.date = SettlementDate)
    
    treasurybonds$ISIN = append(treasurybonds$ISIN, Cusip(Bond))
    treasurybonds$MATURITYDATE = append(treasurybonds$MATURITYDATE, Maturity(Bond))
    treasurybonds$ISSUDATE = append(treasurybonds$ISSUDATE, IssueDate(Bond))
    treasurybonds$COUPONRATE = append(treasurybonds$COUPONRATE, Coupon(Bond))
    treasurybonds$PRICE = append(treasurybonds$PRICE, Price)
    treasurybonds$ACCRUED = append(treasurybonds$ACCRUED,0)
    treasurybonds$CASHFLOWS$ISIN = append(treasurybonds$CASHFLOWS$ISIN,
                                          rep(Cusip(Bond), length(CashFlow[,'TotalCashFlow'])))
    treasurybonds$CASHFLOWS$CF = append(treasurybonds$CASHFLOWS$CF,
                                        unname(CashFlow[,'TotalCashFlow']))
    treasurybonds$CASHFLOWS$DATE = append(treasurybonds$CASHFLOWS$DATE,
                                          unname(as.Date(CashFlow[,'Date'], origin = '1970-01-01')))
    treasurybonds$TODAY = USTData[1,1]
  }
  
  CurveData$treasurybonds <- treasurybonds
  class(CurveData) <- 'couponbonds'
  
  
  #----------------------------------------------------------------------------
  # this is old code that is depricated.  The code was written prior to the 
  # bondlab bond cash flow engines and was used to cashflow swaps for term structure
  # the new code uses the bond lab bond cashflow engine is far more understandable
  #set the column counter to make cashflows for termstrucutre
#  ColCount <- as.numeric(ncol(rates.data))
#  Mat.Years <- as.numeric(rates.data[2,2:ColCount])
#  Coupon.Rate <- as.numeric(rates.data[1,2:ColCount])
#  Issue.Date <- as.Date(rates.data[1,1])
  
  #initialize coupon bonds S3 class
  #This can be upgraded when bondlab has portfolio function
#  ISIN <- vector()
#  MATURITYDATE <- vector()
#  ISSUEDATE <- vector()
#  COUPONRATE <- vector()
#  PRICE <- vector()
#  ACCRUED <- vector()
#  CFISIN <- vector()
#  CF <- vector()
#  DATE <- vector()
#  CASHFLOWS  <- list(CFISIN,CF,DATE)
#  names(CASHFLOWS) <- c("ISIN","CF","DATE")
#  TODAY <- vector()
#  data <- list()
#  TSInput <- list()
  
  ### Assign Values to List Items #########
#  data = NULL
#  data$ISIN <- colnames(rates.data[2:ColCount])
#  data$ISSUEDATE <- rep(as.Date(rates.data[1,1]),ColCount - 1)
  
#  data$MATURITYDATE <-
#    sapply(Mat.Years, function(Mat.Years = Mat.Years, 
#                               Issue = Issue.Date) {
#      Maturity = if(Mat.Years < 1) {
#        Issue %m+% months(round(Mat.Years * months.in.year))
#        } else {Issue %m+% years(as.numeric(Mat.Years))}
#    return(as.character(Maturity))
#    }) 
  
  
#  data$COUPONRATE <- ifelse(Mat.Years < 1, 0, Coupon.Rate)                  
  
#  data$PRICE <- ifelse(Mat.Years < 1, 
#                       (1 + (Coupon.Rate/100))^(Mat.Years * -1) * 100,
#                       100)
  
#  data$ACCRUED <- rep(0, ColCount -1)
  
#  for(j in 1:(ColCount-1)){
#    Vector.Length <- as.numeric(
#      round(difftime(data[[3]][j],
#                     data[[2]][j],
#                     units = c("weeks"))/weeks.in.year,5))
    
#    Vector.Length <- ifelse(round(Vector.Length) < 1, 1 , 
#                            round(Vector.Length * pmt.frequency))
    
#    data$CASHFLOWS$ISIN <- append(data$CASHFLOWS$ISIN, 
#                                  rep(data[[1]][j],Vector.Length))
    
#    data$CASHFLOWS$CF <- append(data$CASHFLOWS$CF,
#      as.numeric(c(rep((data[[4]][j]/100/pmt.frequency), Vector.Length-1) * 
#                     min.principal, 
#              (min.principal + (data$COUPONRATE[j]/100/pmt.frequency)* 
#                 min.principal))))
    
#    by.months = ifelse(data[[4]][j] == 0, 
#                       round(difftime(data[[3]][j], 
#                                      rates.data[1,1])/days.in.month), 6) 
  # this sets the month increment so that cashflows can handle discount bills
  
#  data$CASHFLOWS$DATE <- append(data$CASHFLOW$DATE,
#  seq(as.Date(rates.data[1,1]) %m+% months(as.numeric(by.months)), 
#  as.Date(data[[3]][j]), 
#  by = as.character(paste(by.months, "months", sep = " "))))
    
#  } #The Loop Ends here and the list is made
  
#  data$TODAY <- as.Date(rates.data[1,1])
#  TSInput[[as.character(rates.data[1,1])]] <- c(data)
  
  #set term strucuture input (TSInput) to class couponbonds
#  class(TSInput) <- "couponbonds"
  
#  TS <- TSInput
  # this is end of the old code
  # ----------------------------------------------------------------------------
  
  #Fit the term structure of interest rates
  
  if(method != "cs") {TSFit <- estim_nss(dataset = CurveData, 
                                        group = 'treasurybonds', 
                                        matrange = "all", method = method)
  } else {TSFit <- estim_cs(bonddata = CurveData, 
                     group = 'treasurybonds', 
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
  #period <- seq(from = 1, to = 600, by = 1)
  
  #Use the date from the cashflow file
  #date <- seq(as.Date(rates.data[1,1]) %m+% months(1), 
  #            as.Date(data[[3]][j]), by="1 months")
  
  
  #date.vector <- seq(as.Date(USTData[1,1]), by = 'month', length.out = 600)
  
  
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
 
  time.period <- difftime(seq(as.Date(rates.data[1,1]) %m+% months(1), 
                              by="1 months",
                              length.out = length(spot.rate.curve)),
                          as.Date(rates.data[1,1]),
                          units = "days")/365
  
  date.vector <- seq(as.Date(USTData[1,1]), by = 'month', length.out = 600)

  disc.curve <- round(exp((spot.rate.curve/yield.basis) * -as.numeric(time.period)),8)
  
  # encapsulate function for forward rate.  Forward.Rate function call the 
  # object TermStructure.  forward.tenor is specified in months.
  forward.rate <- function(time.period = time.period,
                           disc.rate = disc.curve,
                           forward.tenor){
    forward.rate = -((log(disc.rate[(forward.tenor + 1) : length(disc.rate)]) -
                       log(disc.rate[1:(length(disc.rate) - forward.tenor)])) / 
      (forward.tenor/months.in.year))
    
    forward.rate <- predict(
      splines::interpSpline(
        seq(1:length(forward.rate)), 
        forward.rate, 
        bSpline = TRUE, 
        na.action = na.omit)
      ,seq(1:length(forward.rate)))$y
    
    forward.rate = forward.rate * yield.basis
    return(forward.rate)
  }

  Two.Year.Fwd <- forward.rate(forward.tenor = 24)[1:600]
  Ten.Year.Fwd <- forward.rate(forward.tenor = 120)[1:600]

  new("TermStructure",
      TradeDate = as.character(rates.data[1,1]),
      Period = seq(1, 600, 1),
      Date = as.character(date.vector),
      TimePeriod = as.numeric(time.period),
      SpotRate = spot.rate.curve,
      ForwardRate = forward.rate.curve,
      DiscRate = disc.curve,
      TwoYearFwd = Two.Year.Fwd,
      TenYearFwd = Ten.Year.Fwd
  )} 
