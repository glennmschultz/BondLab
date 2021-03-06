  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.



  #' @include BondDetails.R MortgageCashFlow.R
  NULL

  
  #' An S4 class representing standard bond cash flows
  #' 
  #' @slot Price a character the price of the bond
  #' @slot Accrued a numeric value the accrued interest
  #' @slot YieldToMaturity a numeric value the bond yield to maturity
  #' @slot WAL a numeric value the weighted average life of the bond
  #' @slot ModDuration a numeric value the bond duration
  #' @slot Convexity a numeric value the bond convexity
  #' @slot Period a numeric value an index of the payment to the investor
  #' @slot PmtDate a character string the payment date to the investor format 
  #' is mm-dd-YYYY
  #' @slot TimePeriod a numeric value the time period between payments made
  #' to the investor
  #' @slot PrincipalOutstanding a numeric value the outstanding principal balance
  #' @slot CouponPmt a numeric value the coupon payment amount
  #' @slot TotalCashFlow a numeric value the sum of the principal and interest
  #' payment made in each period
  #' @exportClass BondCashFlows
  setClass("BondCashFlows",
         representation(
           Price = "character",
           Accrued = "numeric",
           YieldToMaturity = "numeric",
           WAL = "numeric",
           ModDuration = "numeric",
           Convexity = "numeric",
           Period = "numeric",
           PmtDate = "character",
           TimePeriod = "numeric",
           PrincipalOutstanding = "numeric",  
           CouponPmt = "numeric",
           TotalCashFlow = "numeric"))
 
  
  # Note: standard generic Price is defined in MortgageCashFlow.R
  # Note: standard generic Accrued is defined in MortgageCashFlow.R
  # Note: standard generic YieldToMaturity is defined in MortgageCashFlow.R
  # Note: standard generic WAL is defined in MortgageCashFlow.R
  # Note: standard generic ModDuration is defined in MortgageCashFlow.R
  # Note: standard generic Convexity is defined in MortgageCashFlow.R
  # Note: standard generic Period is defined in MortgageCashFlow.R
  # Note: standard generic PmtDate is defined in MortgageCashFlow.R
  # Note: standard generic TimePeriod is defined in MortgageCashFlow.R
  
  #' A standard generic function to get the slot PrincipalOutstanding
  #' @param object An S4 class object
  #' @export PrincipalOutstanding
  setGeneric("PrincipalOutstanding", function(object)
             {standardGeneric("PrincipalOutstanding")})
  
  #' A standard generic function to get the slot CouponPmt
  #' @param object An S4 class object
  #' @export CouponPmt
  setGeneric("CouponPmt", function(object)
    {standardGeneric("CouponPmt")})
  
  # Note: standard generic TotalCashFlow is defined in MortgageCashFlow.R 
  
  setMethod("initialize",
            signature("BondCashFlows"),
            function(.Object,
            Price = "character",
            Accrued = "numeric",
            YieldToMaturity = "numeric",
            WAL = "numeric",
            ModDuration = "numeric",
            Convexity = "numeric",
            Period = "numeric",
            PmtDate = "character",
            TimePeriod = "numeric",
            PrincipalOutstanding = "numeric",  
            CouponPmt = "numeric",
            TotalCashFlow = "numeric",
            ...){
              callNextMethod(.Object,
                             Price = Price,
                             Accrued = Accrued,
                             YieldToMaturity = YieldToMaturity,
                             WAL = WAL,
                             ModDuration = ModDuration,
                             Convexity = Convexity,
                             Period = Period,
                             PmtDate = PmtDate,
                             TimePeriod = TimePeriod,
                             PrincipalOutstanding = PrincipalOutstanding,
                             CouponPmt = CouponPmt,
                             TotalCashFlow = TotalCashFlow,
                             ...)
            })
  
  #' Method to get Price from S4 class
  #' @param object the name of the S4 object BondCashFlows
  #' @exportMethod Price
  setMethod("Price", signature("BondCashFlows"),
            function(object){object@Price})
  
  #' Method to get Accrued from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod Accrued
  setMethod("Accrued", signature("BondCashFlows"),
            function(object){object@Accrued})
  
  #' Method to get YieldToMaturity from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod YieldToMaturity
  setMethod("YieldToMaturity", signature("BondCashFlows"),
            function(object){object@YieldToMaturity})
  
  #' Method to get WAL from S4 class
  #' @param object the name of the S4 object
  #' @exportMethod WAL
  setMethod("WAL", signature("BondCashFlows"),
            function(object){object@WAL})
  
  #' Method to get ModDuration from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod ModDuration
  setMethod("ModDuration", signature("BondCashFlows"),
            function(object){object@ModDuration})
  
  #' Method to get Convexity from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod Convexity
  setMethod("Convexity", signature("BondCashFlows"),
            function(object){object@Convexity})
  
  #' Method to get Period from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod Period
  setMethod("Period", signature("BondCashFlows"),
            function(object){object@Period})
  
  #' Method to get PmtDate from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod PmtDate
  setMethod("PmtDate", signature("BondCashFlows"),
            function(object){object@PmtDate})
  
  #' Method to get TimePeriod from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod TimePeriod
  setMethod("TimePeriod", signature("BondCashFlows"),
            function(object){object@TimePeriod})
  
  #' Method to get PrincipalOutstanding from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod PrincipalOutstanding
  setMethod("PrincipalOutstanding", signature("BondCashFlows"),
            function(object){object@PrincipalOutstanding})
  
  #' Method to get CouponPmt from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod CouponPmt
  setMethod("CouponPmt", signature("BondCashFlows"),
            function(object){object@CouponPmt})
  
  #' Method to get TotalCashFlow from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod TotalCashFlow
  setMethod("TotalCashFlow", signature("BondCashFlows"),
            function(object){object@TotalCashFlow})
  
  
  #' Bond cash flow engine for standard non-callable
  #' 
  #' Cashflow engine for standard non-callable bond
  #' @param bond.id A character string referencing an object of type BondDetails
  #' @param principal A numeric value the principal or face amount of the bond
  #' @param settlement.date A character string the settlement date
  #' @param price A character string the price of the bond
  #' @examples
  #' \dontrun{BondCashFlows(bond.id = "bondlab10", principal = 1000, 
  #' settlement.date = "1-13-2013", price = "100")}
  #' @export BondCashFlows
  #' @importFrom stats uniroot
  BondCashFlows <- function (bond.id = "character", 
                             principal = numeric(), 
                             settlement.date = "character", 
                             price = "character"){

  # Test payment dates against settlement dates and roll forward if payment 
  # settlement date crossses over the payment date
  if(as.Date(settlement.date, format = '%m-%d-%Y') >=
     as.Date(NextPmtDate(bond.id), format = '%m-%d-%Y')){
    bond.id <- `LastPmtDate<-`(bond.id, NextPmtDate(bond.id))}
  
  if(as.Date(LastPmtDate(bond.id), format = '%m-%d-%Y') 
     == as.Date(NextPmtDate(bond.id), format = '%m-%d-%Y')){
    bond.id <- `NextPmtDate<-`(bond.id,
                               as.character(format(
                                 as.Date(LastPmtDate(bond.id), format = "%m-%d-%Y") %m+% 
                                   months(months.in.year/Frequency(bond.id)), 
                                 "%m-%d-%Y")))}
    
    issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
    start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
    end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
    lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
    nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
    coupon = bond.id@Coupon
    frequency = bond.id@Frequency       
    settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
    bondbasis = bond.id@BondBasis
  
  # This function error traps bond input information
  ErrorTrap(bond.id = bond.id, 
            principal = principal,
            settlement.date = settlement.date,
            price = price)
  
  # Pass price to the PriceTypes constructor function.  This function allows
  # converts from 32nds and to decimal basis
  price <- PriceTypes(price = price)
 

  Bond.CF.Table <- CashFlowBond(bond.id = bond.id,
                                principal = principal,
                                settlement.date = settlement.date)
  
  accrual.factor = BondBasisConversion(
    issue.date = issue.date, 
    start.date = NULL, 
    end.date = end.date,
    settlement.date = settlement.date, 
    lastpmt.date = lastpmt.date, 
    nextpmt.date = nextpmt.date, 
    type = bondbasis)

  
  accrued.interest = accrual.factor * Bond.CF.Table[1,"Coupon Income"]

  # Step6 solve for yield to maturity given the price of the bond.  
  # irr is an internal function used to solve for yield to maturity
  # it is internal so that the bond's yield to maturity is not passed to a 
  # global variable that may inadvertantly use the value
  
  irr <- function(rate , 
                  time.period , 
                  cashflow, 
                  principal, 
                  price, 
                  accrued.interest){
    pv = cashflow * (1/(1+(rate/frequency)) ^ (time.period * frequency))
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  
  ytm = uniroot(irr, 
                interval = c(lower = -1, upper = 1), 
                tol = tolerance, 
                time.period = Bond.CF.Table[,"Time"], 
                cashflow = Bond.CF.Table[,"TotalCashFlow"], 
                principal = principal, 
                price = PriceBasis(price), 
                accrued.interest = accrued.interest)$root
  
  # convert to semi-bond equivalent yield
  Yield.To.Maturity = ytm * yield.basis
  
  # pass Yield.To.Maturity to class YieldTypes for conversion to YieldDecimal,
  # YieldBasis, and YieldDecimalString
  Yield <- YieldTypes(yield = Yield.To.Maturity)
  
  #Step7 Present value of the cash flows Present Value Factors
  Bond.CF.Table[,"Present Value Factor"] = 1/
    ((1+(YieldBasis(Yield)/frequency))^(Bond.CF.Table[,"Time"] * frequency))
  
  #Present Value of the cash flows
  Bond.CF.Table[,"Present Value"] = 
    Bond.CF.Table[,"TotalCashFlow"] * Bond.CF.Table[,"Present Value Factor"]
  
  #Step8 Risk measures Duration Factors
  Bond.CF.Table[,"Duration"] = Bond.CF.Table[,"Time"] * 
    (Bond.CF.Table[,"Present Value"]/((principal * PriceBasis(price)) + accrued.interest))
  
  #Convexity Factors
  Bond.CF.Table[,"Convexity Time"] = Bond.CF.Table[,"Time"] *(Bond.CF.Table[,"Time"] + 1)
  Bond.CF.Table[,"CashFlow Convexity"] = (
    Bond.CF.Table[,"TotalCashFlow"]/((1 + ((YieldBasis(Yield))/frequency)) ^ 
                         ((Bond.CF.Table[,"Time"] + 2) * frequency)))/
    ((principal * PriceBasis(price)) + accrued.interest)
  Bond.CF.Table[,"Convexity"] = 
    Bond.CF.Table[,"Convexity Time"] * Bond.CF.Table[,"CashFlow Convexity"] 
  
  #Weighted Average Life
  WAL = sum(
    (Bond.CF.Table[,"Principal Paid"] * Bond.CF.Table[,"Time"]))/
    sum(Bond.CF.Table[,"Principal Paid"])
  #Duration and Convexity
  Duration = apply(Bond.CF.Table, 2, sum)["Duration"]
  Modified.Duration = Duration/(1 + (YieldBasis(Yield)/frequency))
  Convexity = apply(Bond.CF.Table, 2, sum)["Convexity"] * .5

  #Assign Values to the slots
  new("BondCashFlows",   
      Price = PriceDecimalString(price),
      Accrued = accrued.interest,
      YieldToMaturity = YieldDecimal(Yield),
      WAL = WAL,
      ModDuration = unname(Modified.Duration),
      Convexity = unname(Convexity),
      Period = unname(Bond.CF.Table[,"Period"]),
      PmtDate = unname(as.character(as.Date(Bond.CF.Table[,"Date"], origin = "1970-01-01"))),
      TimePeriod = unname(Bond.CF.Table[,"Time"]),
      PrincipalOutstanding  = unname(Bond.CF.Table[,"Principal Outstanding"]),
      CouponPmt = unname(Bond.CF.Table[,"Coupon Income"]),
      TotalCashFlow = unname(Bond.CF.Table[,"TotalCashFlow"])
  )
}


