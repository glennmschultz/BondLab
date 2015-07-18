#Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


#---------------------------------------------------
#Term strucutre call term strc 
#and holds forward and spot rates as slots to class Term Structure
#---------------------------------------------------

# Initialize TermStructure
setMethod("initialize",
          signature("TermStructure"),
          function(.Object,...,
                   tradedate = "character",
                   period = "numeric",
                   date = "character",
                   spotrate = "numeric",
                   forwardrate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric")
          {
            .Object@tradedate = tradedate
            .Object@period = period
            .Object@date = date
            .Object@spotrate = spotrate
            .Object@forwardrate = forwardrate
            .Object@TwoYearFwd = TwoYearFwd
            .Object@TenYearFwd = TenYearFwd
            
            return(.Object)
            callNextMethod(.Object,...)
          })
#' The TermStructure constructor function it is a wrapper function around the package termstrc
#' 
#' This is a wrapper function around the R package termstrc.  The function passes swap rate data
#' cash flows the to termstrc and creates the TermStructure object used by Bondlab.
#' The function call rates data processes the yield curve and derives cashflow
#' for the daily close swap curve. A Rates object must be called in the local
#' environment for this function to work.
#' @param rates.data A character string representing the data for which the user
#' would like to call the swap curve
#' @param method A character string indicating the fitting method ns = Nelson Siegel, dl = Diebold Lee,
#' sv = Severson, asv = adjusted Severson, cs = cubic spline (not yet implemented in Bond Lab).
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
  TermStructure <- function(rates.data = "character", method = "character"){
  
  #function(trade.date = "character", method = "character")  
  #Error Trap User inputs to the function
  if(missing(rates.data)) stop("missing rates data object")  
  
  #Default to Nelson-Siegel
  if(missing(method)) {method = "ns"}
  
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
    sapply(Mat.Years, function(Mat.Years = Mat.Years, Issue = Issue.Date) 
    {Maturity = if(Mat.Years < 1) {Issue %m+% months(round(Mat.Years * months.in.year))} else 
    {Issue %m+% years(as.numeric(Mat.Years))}
    return(as.character(Maturity))
    }) 
  
  data$COUPONRATE <- ifelse(Mat.Years < 1, 0, Coupon.Rate)                  
  
  data$PRICE <-      ifelse(Mat.Years < 1, (1 + (Coupon.Rate/100))^(Mat.Years * -1) * 100, 100)
  
  data$ACCRUED <- rep(0, ColCount -1)
  
  for(j in 1:(ColCount-1)){
    Vector.Length <- as.numeric(round(difftime(data[[3]][j],
                                               data[[2]][j],
                                               units = c("weeks"))/weeks.in.year,0))
    Vector.Length <- ifelse(Vector.Length < 1, 1, Vector.Length * pmt.frequency)  
    #pmt.frequency should be input 
    
    data$CASHFLOWS$ISIN <- append(data$CASHFLOWS$ISIN, rep(data[[1]][j],Vector.Length))
    
    data$CASHFLOWS$CF <- append(data$CASHFLOWS$CF,
              as.numeric(c(rep((data[[4]][j]/100/pmt.frequency), Vector.Length-1) * min.principal, 
              (min.principal + (data$COUPONRATE[j]/100/pmt.frequency)* min.principal))))
    
    by.months = ifelse(data[[4]][j] == 0, round(difftime(data[[3]][j], rates.data[1,1])/days.in.month), 6) 
    # this sets the month increment so that cashflows can handle discount bills
    
    data$CASHFLOWS$DATE <- append(data$CASHFLOW$DATE,
                          seq(as.Date(rates.data[1,1]) %m+% months(as.numeric(by.months)), 
                          as.Date(data[[3]][j]), by = as.character(paste(by.months, "months", sep = " "))))
    
  } #The Loop Ends here and the list is made
  
  data$TODAY <- as.Date(rates.data[1,1])
  TSInput[[as.character(rates.data[1,1])]] <- c(data)
  
  #set term strucuture input (TSInput) to class couponbonds
  class(TSInput) <- "couponbonds"
  
  #Fit the term structure of interest rates
  
  if(method != "cs") {TSFit <- estim_nss(dataset = TSInput, 
                                         group = as.character(rates.data[1,1]), 
                                         matrange = "all", method = method)} else
  {TSFit <- estim_cs(bonddata = TSInput, 
                     group = as.character(rates.data[1,1]), 
                     matrange = "all", rse = TRUE)}
  
  #Return the coefficient vector to be passed in to the spot and forward rate functions
  #Maybe have the method choosen based on the one that gives the smallest RMSE
  Vector <- switch(method,
                   ns = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1")]),
                   dl = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2")]),
                   sv = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1", "beta3", "tau2")]),
                   asv = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1", "tau2", "tau3")]),
                   #cs = need to figure this out
  )
  
  #Calculate the spot rate curve and determine the forward rates needed to 
  period <- seq(from = 1, to = 492, by = 1)
  #Use the date from the cashflow file
  date <- seq(as.Date(rates.data[1,1]) %m+% months(1), as.Date(data[[3]][j]), by="1 months")
  
  spot.rate.curve <- spotrates(method = method, beta = Vector, m = seq(from = 1/12, to = 492/12, by = 1/12))
  
  forward.rate.curve <- forwardrates(method = method, beta = Vector, m = seq(from = 1/12, to = 492/12, by = 1/12))
  
  Two.Year.Fwd <- (((1 + spot.rate.curve[seq(from = 25, to = 385, by = 1)]) ^ 
                      (period[seq(from = 25, to = 385, by = 1)]/12) /
                      (1 + spot.rate.curve[seq(from = 1, to = 361, by = 1)]) ^ 
                      (period[seq(from = 1, to = 361, by = 1)]/12))^(1/2))-1
  
  Ten.Year.Fwd <- (((1 + spot.rate.curve[seq(from = 121, to = 481, by = 1)]) ^ 
                      (period[seq(from = 121, to = 481, by = 1)]/12) /
                      (1 + spot.rate.curve[seq(from = 1, to = 361, by = 1)]) ^ 
                      (period[seq(from = 1, to = 361, by = 1)]/12))^(1/10))-1
  
  new("TermStructure",
      tradedate = as.character(rates.data[1,1]),
      period = as.numeric(period),
      date = as.character(date),
      spotrate = spot.rate.curve,
      forwardrate = forward.rate.curve,
      TwoYearFwd = Two.Year.Fwd,
      TenYearFwd = Ten.Year.Fwd
  )
} 


setGeneric("TermStructure",
           function(rates.data = "character", method = "character")
           {standardGeneric("TermStructure")})