  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + license
  # Copyright (C) 2014  Bond Lab Technologies, Inc
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


  setMethod("initialize",
            signature("BondCashFlows"),
            function(.Object,
            Price = "numeric",
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
            TotalCashFlow = "numeric")
            
            {
              .Object@Price = Price
              .Object@Accrued = Accrued
              .Object@YieldToMaturity = YieldToMaturity
              .Object@WAL = WAL
              .Object@ModDuration = ModDuration
              .Object@Convexity = Convexity
              .Object@Period = Period
              .Object@PmtDate = PmtDate
              .Object@TimePeriod = TimePeriod
              .Object@PrincipalOutstanding = PrincipalOutstanding  
              .Object@CouponPmt = CouponPmt
              .Object@TotalCashFlow = TotalCashFlow
              
              return(.Object)
              callNextMethod(.Object,...)
            })
  #' Bond cash flow engine for standard non-callable
  #' 
  #' Cashflow engine for standard non-callable bond
  #' @param bond.id A character string the cusip number or id of the bond
  #' @param principal A numeric value the principal or face amount of the bond
  #' @param settlement.date A character string the settlement date
  #' @param price A numeric value the price of the bond
  #' @examples
  #' \dontrun{BondCashFlows(bond.id = "bondlab10", principal = 1000, 
  #' settlement.date = "1-13-2013", price = 100)}
  #' @export
  BondCashFlows <- function (bond.id = "character", 
                             principal = numeric(), 
                             settlement.date = "character", 
                             price = numeric()){
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency       
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  
  # This function error traps bond input information
  
  ErrorTrap(bond.id = bond.id, 
            principal = principal,
            settlement.date = settlement.date,
            price = price)
  
  #  Validate the price and coupon passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(price <= 1) {price = price} else {price = price/100}
  if(coupon > 1) {coupon = coupon/100} else {coupon = coupon}
  
  #Calculate the number of cashflows that will be paid from settlement date to maturity date 
  #step1 calculate the years to maturity  
  ncashflows = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                   lastpmt.date = lastpmt.date, nextpmt.date = end.date) 
  
  #Step2 build a vector of dates for the payment schedule
  # first get the pmtdate interval
  pmtdate.interval = months.in.year/frequency
  # then compute the payment dates
  pmtdate = as.Date(c(if(settlement.date == issue.date) {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
                      else {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y")
  
  #step3 build the time period vector (n) for discounting the cashflows nextpmt date is vector of payment dates to n for each period
  time.period = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, nextpmt.date = pmtdate)
  
  #step4 Count the number of cashflows 
  #num.periods is the total number of cashflows to be received
  #num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  col.names <- c("Period", "Date", "Time", "Principal Outstanding", "Coupon", "Coupon Income", "Principal Paid", "TotalCashFlow",
                 "Present Value Factor", "Present Value", "Duration", "Convexity Time", "CashFlow Convexity", "Convexity")
  
  Bond.CF.Table <- array(data = NA, c(num.periods, 14), dimnames = list(seq(c(1:num.periods)),col.names))  
  for(i in 1:num.periods){
    Bond.CF.Table[i,1] = i
    Bond.CF.Table[i,2] = pmtdate[i]
    Bond.CF.Table[i,3] = time.period[i]
    Bond.CF.Table[i,4] = principal
    Bond.CF.Table[i,5] = coupon /frequency
    Bond.CF.Table[i,6] = Bond.CF.Table[i,5] * Bond.CF.Table[i,4]
    if(Bond.CF.Table[i,2] == end.date) {Bond.CF.Table[i,7] = principal} else {Bond.CF.Table[i,7] = 0}
    Bond.CF.Table[i,8] = Bond.CF.Table[i,6] + Bond.CF.Table[i,7]
  }
  
  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, 
                                         settlement.date = settlement.date, lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date)) * 360
  
  days.between.pmtdate = ((months.in.year/frequency)/months.in.year) * days.in.year.360
  days.of.accrued = days.between.pmtdate - days.to.nextpmt
  accrued.interest = (days.of.accrued/days.between.pmtdate) * Bond.CF.Table[1,6]
  
  #Step6 solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  #it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value 
  irr <- function(rate , time.period , cashflow , principal , price , accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol =.000000001, time.period = Bond.CF.Table[,3], 
                cashflow = Bond.CF.Table[,8], principal = principal, price = price, accrued.interest = accrued.interest)$root
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency
  
  #Step7 Present value of the cash flows Present Value Factors
  Bond.CF.Table[,9] = 1/((1+(Yield.To.Maturity/frequency))^(Bond.CF.Table[,3] * frequency))
  
  #Present Value of the cash flows
  Bond.CF.Table[,10] = Bond.CF.Table[,8] * Bond.CF.Table[,9]
  
  #Step8 Risk measures Duration Factors
  Bond.CF.Table[,11] = Bond.CF.Table[,3] * (Bond.CF.Table[,10]/((principal * price) + accrued.interest))
  
  #Convexity Factors
  Bond.CF.Table[,12] = Bond.CF.Table[,3] *(Bond.CF.Table[,3] + 1)
  Bond.CF.Table[,13] = (Bond.CF.Table[,8]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((Bond.CF.Table[,3] + 2) * frequency)))/((principal * price) + accrued.interest)
  Bond.CF.Table[,14] = Bond.CF.Table[,12] * Bond.CF.Table[,13] 
  
  #Weighted Average Life
  WAL = sum((Bond.CF.Table[,7] * Bond.CF.Table[,3]))/sum(Bond.CF.Table[,7])
  #Duration and Convexity
  Duration = apply(Bond.CF.Table, 2, sum)[11]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(Bond.CF.Table, 2, sum)[14] * .5
  
  #Assign Values to the slots
  new("BondCashFlows",   
      Price = price * 100,
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

  setGeneric("BondCashFlows", function (bond.id = "character", 
                                        principal = numeric(), 
                                        settlement.date = "character", 
                                        price = numeric())
  {standardGeneric("BondCashFlows")})
