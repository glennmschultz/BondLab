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


  #'@include MortgageCashFlow.R
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
  #' is mm/dd/YYYY
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
 
  setGeneric("BondCashFlows", function (bond.id = "character", 
                                      principal = numeric(), 
                                      settlement.date = "character", 
                                      price = "character")
  {standardGeneric("BondCashFlows")})
  
  # Note: standard generic Price is defined in MortgageCashFlow.R
  # Note: standard generic Accrued is defined in MortgageCashFlow.R
  # Note: standard generic YieldToMaturity is defined in MortgageCashFlow.R
  # Note: standard generic WAL is defined in MortgageCashFlow.R
  # Note: standard generic ModDuration is defined in MortgageCashFlow.R
  # Note: standard generic Convexity is defined in MortgageCashFlow.R
  # Note: standard generic Period is defined in MortgageCashFlow.R
  # Note: standard generic PmtDate is defined in MortgageCashFlow.R
  # Note: standard generic TimePeriod is defined in MortgageCashFlow.R
  
  #' A standard generic function to access the slot PrincipalOutstanding
  #' @param object An S4 class object
  #' @export PrincipalOutstanding
  setGeneric("PrincipalOutstanding", function(object)
             {standardGeneric})
  
  #' A standard generic function to access the slot CouponPmt
  #' @param object An S4 class object
  #' @export CouponPmt
  setGeneric("CouponPmt", function(object)
    {standardGeneric})
  
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
  
  #' Method to extract Price from S4 class
  #' @param object the name of the S4 object BondCashFlows
  #' @exportMethod Price
  setMethod("Price", signature("BondCashFlows"),
            function(object){object@Price})
  
  #' Method to extract Accrued from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod Accrued
  setMethod("Accrued", signature("BondCashFlows"),
            function(object){object@Accrued})
  
  #' Method to extract YieldToMaturity from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod YieldToMaturity
  setMethod("YieldToMaturity", signature("BondCashFlows"),
            function(object){object@YieldToMaturity})
  
  #' Method to extract WAL from S4 class
  #' @param object the name of the S4 object
  #' @exportMethod WAL
  setMethod("WAL", signature("BondCashFlows"),
            function(object){object@WAL})
  
  #' Method to extract ModDuration from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod ModDuration
  setMethod("ModDuration", signature("BondCashFlows"),
            function(object){object@ModDuration})
  
  #' Method to extract Convexity from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod Convexity
  setMethod("Convexity", signature("BondCashFlows"),
            function(object){object@Convexity})
  
  #' Method to extract Period from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod Period
  setMethod("Period", signature("BondCashFlows"),
            function(object){object@Period})
  
  #' Method to extract PmtDate from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod PmtDate
  setMethod("PmtDate", signature("BondCashFlows"),
            function(object){object@PmtDate})
  
  #' Method to extract TimePeriod from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod TimePeriod
  setMethod("TimePeriod", signature("BondCashFlows"),
            function(object){object@TimePeriod})
  
  #' Method to extract PrincipalOutstanding from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod PrincipalOutstanding
  setMethod("PrincipalOutstanding", signature("BondCashFlows"),
            function(object){object@PrincipalOutstanding})
  
  #' Method to extract CouponPmt from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod CouponPmt
  setMethod("CouponPmt", signature("BondCashFlows"),
            function(object){object@CouponPmt})
  
  #' Method to extract TotalCashFlow from S4 class BondCashFlows
  #' @param object the name of the S4 object
  #' @exportMethod TotalCashFlow
  setMethod("TotalCashFlow", signature("BondCashFlows"),
            function(object){object@TotalCashFlow})
  
  
  #' Bond cash flow engine for standard non-callable
  #' 
  #' Cashflow engine for standard non-callable bond
  #' @param bond.id A character string the cusip number or id of the bond
  #' @param principal A numeric string the principal or face amount of the bond
  #' @param settlement.date A character string the settlement date
  #' @param price A character value the price of the bond
  #' @examples
  #' \dontrun{BondCashFlows(bond.id = "bondlab10", principal = 1000, 
  #' settlement.date = "1-13-2013", price = "100")}
  #' @export BondCashFlows
  BondCashFlows <- function (bond.id = "character", 
                             principal = numeric(), 
                             settlement.date = "character", 
                             price = "character"){
  
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
  price <- PriceTypes(Price = price)
  coupon = coupon/yield.basis

  # Calculate the number of cashflows that will be paid from settlement date to
  # maturity date 
  # step1 calculate the years to maturity  
  ncashflows = BondBasisConversion(
    issue.date = issue.date, 
    start.date = start.date, 
    end.date = end.date, 
    settlement.date = settlement.date,
    lastpmt.date = lastpmt.date, 
    nextpmt.date = end.date, 
    type = bondbasis) 
  
  #Step2 build a vector of dates for the payment schedule
  # first get the pmtdate interval
  pmtdate.interval = months.in.year/frequency
  
  # then compute the payment dates
  pmtdate = as.Date(c(if(settlement.date == issue.date) {
    seq(start.date, end.date, by = paste(pmtdate.interval, "months"))
    } else {
      seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), 
    "%m-%d-%Y")
  
  #step3 build the time period vector (n) for discounting the cashflows 
  #nextpmt date is vector of payment dates to n for each period
  time.period = BondBasisConversion(
    issue.date = issue.date, start.date = start.date, end.date = end.date, 
    settlement.date = settlement.date, lastpmt.date = lastpmt.date, 
    nextpmt.date = pmtdate, type = bondbasis)
  
  #step4 Count the number of cashflows 
  #num.periods is the total number of cashflows to be received
  #num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  col.names <- c("Period", 
                 "Date", 
                 "Time", 
                 "Principal Outstanding", 
                 "Coupon", 
                 "Coupon Income", 
                 "Principal Paid", 
                 "TotalCashFlow",
                 "Present Value Factor", 
                 "Present Value", 
                 "Duration", 
                 "Convexity Time", 
                 "CashFlow Convexity", 
                 "Convexity")
  
  Bond.CF.Table <- array(data = NA, c(num.periods, 14), 
                         dimnames = list(seq(c(1:num.periods)),col.names))  
  for(i in 1:num.periods){
    Bond.CF.Table[i,1] = i
    Bond.CF.Table[i,2] = pmtdate[i]
    Bond.CF.Table[i,3] = time.period[i]
    Bond.CF.Table[i,4] = principal
    Bond.CF.Table[i,5] = coupon /frequency
    Bond.CF.Table[i,6] = Bond.CF.Table[i,5] * Bond.CF.Table[i,4]
    if(Bond.CF.Table[i,2] == end.date) {Bond.CF.Table[i,7] = principal
    } else {Bond.CF.Table[i,7] = 0}
    Bond.CF.Table[i,8] = Bond.CF.Table[i,6] + Bond.CF.Table[i,7]
  }
  
  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(
    issue.date = issue.date, 
    start.date = start.date, 
    end.date = end.date,
    settlement.date = settlement.date, 
    lastpmt.date = lastpmt.date, 
    nextpmt.date = nextpmt.date, type = bondbasis)) * days.in.year.360
  
  days.between.pmtdate = ((
    months.in.year/frequency)/months.in.year) * days.in.year.360
  days.of.accrued = days.between.pmtdate - days.to.nextpmt
  accrued.interest = (days.of.accrued/days.between.pmtdate) * Bond.CF.Table[1,6]


  # Step6 solve for yield to maturity given the price of the bond.  
  # irr is an internal function used to solve for yield to maturity
  # it is internal so that the bond's yield to maturity is not passed to a 
  # global variable that may inadvertantly use the value
  
  irr <- function(rate , 
                  time.period , 
                  cashflow , 
                  principal , 
                  price , 
                  accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  
  ytm = uniroot(irr, 
                interval = c(lower = -1, upper = 1), 
                tol =.000000001, 
                time.period = Bond.CF.Table[,3], 
                cashflow = Bond.CF.Table[,8], 
                principal = principal, 
                price = PriceBasis(price), 
                accrued.interest = accrued.interest)$root
  
  # convert to semi-bond equivalent yield
  Yield.To.Maturity = (((1 + ytm)^(1/2))-1) * 2
  
  #Step7 Present value of the cash flows Present Value Factors
  Bond.CF.Table[,9] = 1/
    ((1+(Yield.To.Maturity/frequency))^(Bond.CF.Table[,3] * frequency))
  
  #Present Value of the cash flows
  Bond.CF.Table[,10] = Bond.CF.Table[,8] * Bond.CF.Table[,9]
  
  #Step8 Risk measures Duration Factors
  Bond.CF.Table[,11] = Bond.CF.Table[,3] * 
    (Bond.CF.Table[,10]/((principal * PriceBasis(price)) + accrued.interest))
  
  #Convexity Factors
  Bond.CF.Table[,12] = Bond.CF.Table[,3] *(Bond.CF.Table[,3] + 1)
  Bond.CF.Table[,13] = (Bond.CF.Table[,8]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((Bond.CF.Table[,3] + 2) * frequency)))/((principal * PriceBasis(price)) + accrued.interest)
  Bond.CF.Table[,14] = Bond.CF.Table[,12] * Bond.CF.Table[,13] 
  
  #Weighted Average Life
  WAL = sum((Bond.CF.Table[,7] * Bond.CF.Table[,3]))/sum(Bond.CF.Table[,7])
  #Duration and Convexity
  Duration = apply(Bond.CF.Table, 2, sum)[11]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(Bond.CF.Table, 2, sum)[14] * .5
  
  #Assign Values to the slots
  new("BondCashFlows",   
      Price = PriceDecimalString(price),
      Accrued = accrued.interest,
      YieldToMaturity = Yield.To.Maturity,
      WAL = WAL,
      ModDuration = Modified.Duration,
      Convexity = Convexity,
      Period = Bond.CF.Table[,1],
      PmtDate = as.character(as.Date(Bond.CF.Table[,2], origin = "1970-01-01")),
      TimePeriod = Bond.CF.Table[,3],
      PrincipalOutstanding  = Bond.CF.Table[,4],
      CouponPmt = Bond.CF.Table[,5],
      TotalCashFlow = Bond.CF.Table[,8]
  )
}


