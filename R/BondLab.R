# BondLab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License MIT

#Copyright (C) 2014  Glenn M Schultz, CFA
  
  options(digits = 8)
  library(termstrc)
  library(ggplot2)
  library(reshape2)
  library(lubridate)
  library(methods)
  library(plyr)

  #----------------------------------------------------------------------------------------
  # Utils globalVariables is called so that the R CMD check will not issue a note
  # The utils below are global variables for the multi plot function used for ggplot2 methods
  utils::globalVariables(c("grid.newpage", "pushViewport", "viewport", "gridlayout"))
  # The utils below are global variables for the BondAnalytics and PassThroughAnalytics methods using ggplot2
  utils::globalVariables(c("Period", "CashFlow", "KRTenor", "KRDuration", "Period", "CashFlow"))
  # The utils below are global variables for the MortgageCashFlow methods using ggplot2
  utils::globalVariables(c("value", "variable"))

  #----------------------------------------------------------------------------------------
  #Bond Lab Functions
  #----------------------------------------------------------------------------------------

  #---------------------------------
  #Time value of money functions
  #---------------------------------

  PresentValue <- function(interest.rate = numeric(), number.periods = numeric(), frequency = numeric()) {
  
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate <0 | interest.rate > 1)
    stop("No valid  interest.rate specified.")
  
  if(missing(number.periods))
    stop("Need to provide the number of discounting periods")
  if(!is.numeric(number.periods))
    stop(" No numeric discounting period specfified")
  if (number.periods < 1 )
    stop("No valid  number.periods specified.")
  
  if(missing(frequency))
    stop("Need to provide the frequency")
  if(!is.numeric(frequency))
    stop(" No numeric frequency specfified")
  if (frequency < 1 | frequency >12 )
    stop("No valid frequency specified.")
  
  interest.rate = interest.rate/frequency
  
  1/(1+interest.rate)^number.periods
  }

  FutureValue <- function(interest.rate = numeric(), number.periods = numeric(), frequency = numeric()) {
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate <0 | interest.rate > 1)
    stop("No valid  interest.rate specified.")
  
  if(missing(number.periods))
    stop("Need to provide the number of discounting periods")
  if(!is.numeric(number.periods))
    stop(" No numeric discounting period specfified")
  if (number.periods < 1 )
    stop("No valid  number.periods specified.")
  
  if(missing(frequency))
    stop("Need to provide the frequency")
  if(!is.numeric(frequency))
    stop(" No numeric frequency specfified")
  if (frequency < 1 | frequency >12 )
    stop("No valid frequency specified.")
  
  interest.rate = interest.rate/frequency
  
  (1+interest.rate)^number.periods  
  } 

  PresentValueAnnuity <-function(interest.rate = numeric(), number.periods = numeric(), frequency = numeric()) {
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate <0 | interest.rate > 1)
    stop("No valid  interest.rate specified.")
  if(missing(number.periods))
    stop("Need to provide the number of discounting periods")
  if(!is.numeric(number.periods))
    stop(" No numeric discounting period specfified")
  if (number.periods < 1 )
    stop("No valid  number.periods specified.")
  if(missing(frequency))
    stop("Need to provide the frequency")
  if(!is.numeric(frequency))
    stop(" No numeric frequency specfified")
  if (frequency < 1 | frequency >12 )
    stop("No valid frequency specified.")
  interest.rate = interest.rate/frequency
  ((1-(1/(1+interest.rate)^number.periods))/interest.rate)
  }

  PresentValueAnnuityDue <-function(interest.rate = numeric(), number.periods = numeric(), frequency = numeric()){
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate <0 | interest.rate > 1)
    stop("No valid  interest.rate specified.")
  if(missing(number.periods))
    stop("Need to provide the number of discounting periods")
  if(!is.numeric(number.periods))
    stop(" No numeric discounting period specfified")
  if (number.periods < 1 )
    stop("No valid  number.periods specified.")
  if(missing(frequency))
    stop("Need to provide the frequency")
  if(!is.numeric(frequency))
    stop(" No numeric frequency specfified")
  if (frequency < 1 | frequency >12 )
    stop("No valid frequency specified.")
  interest.rate = interest.rate/frequency
  ((1-(1/(1+interest.rate)^number.periods))/interest.rate) * (1+interest.rate)  
  }

  FutureValueAnnuity <- function(interest.rate = numeric(), number.periods = numeric(), frequency = numeric()){
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate <0 | interest.rate > 1)
    stop("No valid  interest.rate specified.")
  
  if(missing(number.periods))
    stop("Need to provide the number of discounting periods")
  if(!is.numeric(number.periods))
    stop(" No numeric discounting period specfified")
  if (number.periods < 1 )
    stop("No valid  number.periods specified.")
  
  if(missing(frequency))
    stop("Need to provide the frequency")
  if(!is.numeric(frequency))
    stop(" No numeric frequency specfified")
  if (frequency < 1 | frequency >12 )
    stop("No valid frequency specified.")
  
  interest.rate = interest.rate/frequency
  (((1 + interest.rate)^(number.periods)) -1)/interest.rate
  }

  #----------------------------------
  # Bond Yield to Maturity Functions
  #---------------------------------

  #YYTMtoPrice is a simple yield to price equation for a standard bond
  #This equation treats the bond as a annuity and zero coupon payment
  YTMtoPrice<- function(yield.to.maturity = numeric() ,coupon = numeric(), coupon.frequency = numeric(), 
                        years.mat = numeric(), face.value = numeric()){
    
    if (missing(yield.to.maturity))
    stop("Need to specify a maturity greater than 0")
    if (!is.numeric(yield.to.maturity)  )
    stop("No numeric yield to maturity specified.")
    if (yield.to.maturity <0 | yield.to.maturity > 1)
    stop("No valid  interest.rate specified.")
  
  #need to error trap this function
  n = years.mat * coupon.frequency
  c = coupon/coupon.frequency
  i = yield.to.maturity/coupon.frequency
  fv = face.value
  
  (((1-(1/(1+i)^n))/i) * (fv * c)) + (1/(1+i)^n * fv)   
  }

  #bondprice is a simple price to yield equation for a standard bond
  #This equation treats the bond as a annuity and zero coupon payment
  bondprice<- function(yield.to.maturity = numeric(),coupon = numeric(), coupon.frequency = numeric(), years.mat = numeric(), face.value = numeric()){
  n = years.mat * coupon.frequency
  c = coupon/coupon.frequency
  i = yield.to.maturity/coupon.frequency
  fv = face.value
  
  (((1-(1/(1+i)^n))/i) * (fv * c)) + (1/(1+i)^n * fv)
  }

  #bond estimated yield to maturity this equation estimates a bond's yield
  #to maturity for a yield to maturity guess may be used with uniroot ytm functions
  EstimYTM <- function(coupon = numeric(), coupon.frequency = numeric(), years.mat = numeric(), face.value = numeric(), price = numeric()){
  c = coupon
  n = years.mat
  f = coupon.frequency
  fv = face.value
  p = price/100 
  ((c * fv) + ((fv - (fv *p))/2)) / (((fv + (fv *p))/f))
  }

  #---------------------------
  # Mortgage Payment Functions
  #---------------------------

  Mortgage.Monthly.Payment <- function(orig.bal = numeric(), note.rate = numeric(), term.mos = numeric()){
  
  #Error Trap Note Rate
    if (missing(note.rate))
      stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
    if (!is.numeric(note.rate)  )
      stop("No numeric interest.rate specified.")

  note.rate = note.rate/12 
  term = term.mos
  pmt.factor = (1+note.rate)^term
  pmt = (orig.bal * pmt.factor) * (note.rate/(pmt.factor -1))
  pmt
  }

  Sched.Prin <- function(balance = numeric(), note.rate = numeric(), term.mos = numeric(), period = numeric()){
  note.rate = note.rate/12
  term = term.mos
  disc.pmt =  note.rate * (1+note.rate)^(period-1)
  disc.prin = ((1+note.rate)^(term))-1
  Scheduled.Prin = balance *(disc.pmt/disc.prin)
  Scheduled.Prin
  }

  Remain.Balance <- function(balance = numeric(), note.rate = numeric(), term.mos = numeric(), period = numeric()){
  note.rate = note.rate/1200
  term = term.mos
  Remain.Balance = balance * ((((1+note.rate)^term) - ((1+note.rate)^period))/(((1+note.rate)^term.mos)-1))
  Remain.Balance
  }

  PPC.Ramp <- function(season.period = numeric(), begin.cpr = numeric(), end.cpr = numeric(), period = numeric()){
  if(end.cpr >= 1) {end.cpr = end.cpr/100 
                    begin.cpr = begin.cpr/100}
  monthly.cpr = (begin.cpr + ((period - 1) * (end.cpr-begin.cpr)/(season.period -1)))
  cpr = ifelse(monthly.cpr <= end.cpr, monthly.cpr, end.cpr)
  cpr
  }

  #--------------------------
  # Error trap function for bond inputs
  #--------------------------  
  ErrorTrap <- function(bond.id = "character", principal = numeric(), settlement.date = "character", price = numeric()) {
  #Error inputs by the user make sure all needed dates and values are passed to Bond Cash Flow
  issue.date = bond.id@IssueDate
  start.date = bond.id@DatedDate
  end.date = bond.id@Maturity
  lastpmt.date = bond.id@LastPmtDate
  nextpmt.date = bond.id@NextPmtDate
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  
  # Test Bond Inputs
  if(missing(issue.date)) stop("missing issue date")
  if(missing(start.date)) stop("missing start date")
  if(missing (end.date)) stop("missing end (Maturity) date")
  if(missing(lastpmt.date)) stop("missing last payment date")
  if(missing (nextpmt.date)) stop("missing next payment date")
  if(missing (coupon)) stop("missing coupon")
  if(missing (frequency)) stop("missing frequency")
  #Test Trade Inputs
  if (missing (settlement.date)) stop("missing settlement date")
  if(missing (principal)) stop("missing principal")
  if(missing (price)) stop("missing price")
  
  issue.date = as.Date(c(issue.date), "%m-%d-%Y")
  start.date = as.Date(c(start.date), "%m-%d-%Y")
  end.date = as.Date(c(end.date), "%m-%d-%Y")
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  lastpmt.date = as.Date(c(lastpmt.date), "%m-%d-%Y")
  nextpmt.date = as.Date(c(nextpmt.date), "%m-%d-%Y")
  
  #============= Pass dates into the function m-d-y ======================
  
  #Error Trapping on Dates settlement date < next payment date or settlement date , issue date    
  if(settlement.date > nextpmt.date) stop ("Settlement Date is Greater Than Next Payment Date")
  if(settlement.date < issue.date) stop ("Settlement Date is Less Than Issue Date")
  if(settlement.date < lastpmt.date) stop ("Settlment Date is Less Than Last Payment Date")
  #Error Trapping on frequency and payment dates
  
  #Error Trap the user's price input
  if(price <= 0) stop("No valid bond price")
  #Error Trap the user's coupon input
  if (coupon < 0 | coupon > 100) stop("No valid coupon specified.")
  #Note: Minimum demonination needs to be added to class bond information as well as function input
  }

  #----------------------------
  #Bond basis function This function set the interest payment day count basis  
  #----------------------------

  BondBasisConversion <- function(issue.date, start.date, end.date, settlement.date, 
                        lastpmt.date, nextpmt.date){
  # This  Function converts day count to bond U.S. Bond Basis 30/360 day count calculation 
  #It returns the number of payments that will be received, period, and n for discounting
  #issue.date is the issuance date of the bond
  #start.date is the dated date of the bond
  #end.date is the maturity date of the bond
  #settlement.date is the settlement date of hte bond
  #lastpmt.date is the last coupon payment date
  #nextpmt.date is the next coupon payment date

  d1 = if(settlement.date == issue.date) {day(issue.date)} else {day(settlement.date)}    
  m1 = if(settlement.date == issue.date) {month(issue.date)} else {month(settlement.date)}
  y1 = if(settlement.date == issue.date) {year(issue.date)} else {year(settlement.date)}
  d2 = day(nextpmt.date)
  m2 = month(nextpmt.date)
  y2 = year(nextpmt.date)
  
  (max(0, 30 - d1) + min(30, d2) + 360*(y2-y1) + 30*(m2-m1-1))/360}

  #--------------------------
  #Bond cash flow function. This function computes the cash flow of a standard non-callable bond
  #-------------------------
  BondCashFlows <- function (bond.id = "character", principal = numeric(), settlement.date = "character", price = numeric()){
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency       
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  
  # This function error traps bond input information
  ErrorTrap(bond.id = bond.id, principal = principal, settlement.date = settlement.date, price = price)
  
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
  pmtdate.interval = 12/frequency
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
  
  days.between.pmtdate = ((12/frequency)/12) * 360
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
      TotalCashFlow = Bond.CF.Table[,8],
      bond.id
  )
  }
  #------------------------------------------------------
  #Mortgage cash flow function.  This function calculates the cash flow of a mortgage pass through security
  #-----------------------------------------------------
  MortgageCashFlows <- function(bond.id = "character", original.bal = numeric(), settlement.date = "character", 
                              price = numeric(), PrepaymentAssumption = "character"){
   
  #This function error traps mortgage bond inputs
  ErrorTrap(bond.id = bond.id, principal = original.balance, settlement.date = settlement.date, price = price)
  
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  delay = bond.id@PaymentDelay
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  
  #Mortgage specific inputs
  note.rate = bond.id@GWac
  FirstPmtDate = bond.id@FirstPmtDate
  FinalPmtDate = bond.id@FinalPmtDate
  servicing.fee = bond.id@Servicing
  pmi = bond.id@PMI
  g.fee = bond.id@Gfee
  
  #error trap function needs to be upated to reflect expansion of the bond class
  #needs servicing, pmi and gfee error trapping
  
  #  Validate the price and coupon passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(price <= 1) {price = price} else {price = price/100}
  if(coupon > 1) {coupon = coupon/100} else {coupon = coupon}
  if(note.rate > 1) {note.rate = note.rate/100} else {note.rate = note.rate}
  
  #calcualte beginning balance (principal) from the MBS pool factor
  factor = bond.id@MBSFactor
  principal = original.bal * factor
  
  #Calculate the number of cashflows that will be paid from settlement date to the last pmt date
  #step 1 calculate the years to maturity
  ncashflows = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                   lastpmt.date = lastpmt.date, nextpmt.date = end.date) 
  
  #Step2 build a vector of dates for the payment schedule
  # first get the pmtdate interval
  pmtdate.interval = 12/frequency
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
  
  #step5 initialize the prepayment model assumption class
  
  col.names <- c("Period", "Date", "Time", "Begin Bal", "Monthly Pmt", "Scheduled Int", "Scheduled Prin", "Prepaid Prin", 
                 "Ending Bal", "Sevicing", "PMI", "GFee", "Pass Through Interest", "Investor CashFlow", "Present Value Factor", "Present Value", 
                 "Duration", "Convexity Time", "CashFlow Convexity", "Convexity")
  
  
  MBS.CF.Table <- array(data = NA, c(num.periods, 20), dimnames = list(seq(c(1:num.periods)),col.names))  
  for(x in 1:num.periods){
    MBS.CF.Table[x,1] = x
    MBS.CF.Table[x,2] = pmtdate[x] + delay
    MBS.CF.Table[x,3] = time.period[x]
    if (MBS.CF.Table[x,1] == 1) {MBS.CF.Table[x,4] = principal} else {MBS.CF.Table[x,4] = MBS.CF.Table[x-1,9]}
    MBS.CF.Table[x,5] = Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x,4], note.rate = note.rate, 
                                                 term.mos = (num.periods - MBS.CF.Table[x,1] + 1))
    
    MBS.CF.Table[x,6] = MBS.CF.Table[x,4] * (note.rate/12)
    MBS.CF.Table[x,7] = Sched.Prin(balance = MBS.CF.Table[x,4], note.rate = note.rate, 
                                   term.mos = (num.periods - MBS.CF.Table[x,1] + 1), period = 1)
        
    if(x != num.periods) {MBS.CF.Table[x,8] = PrepaymentAssumption@SMM[x] * (MBS.CF.Table[x,4] - MBS.CF.Table[x,7])} else                     
      {MBS.CF.Table[x,8] = 0}
    
    MBS.CF.Table[x,9] = MBS.CF.Table[x,4] - MBS.CF.Table[x,7] - MBS.CF.Table[x,8]
    MBS.CF.Table[x,10] = MBS.CF.Table[x,4] * (servicing.fee/1200)
    MBS.CF.Table[x,11] = MBS.CF.Table[x,4] * (pmi/1200)
    MBS.CF.Table[x,12] = MBS.CF.Table[x,4] * (g.fee/1200)
    MBS.CF.Table[x,13] = MBS.CF.Table[x,4] * (coupon/12)
    MBS.CF.Table[x,14] = MBS.CF.Table[x,13] + MBS.CF.Table[x,7] + MBS.CF.Table[x,8]
  }
  
  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, 
                        settlement.date = settlement.date, lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date)) * 360
 
  days.between.pmtdate = ((12/frequency)/12) * 360
  days.of.accrued = (days.between.pmtdate - days.to.nextpmt) 
  accrued.interest = (days.of.accrued/days.between.pmtdate) * MBS.CF.Table[1,13]
  
  #Step6 solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  #it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value 
  irr <- function(rate , time.period , cashflow , principal , price , accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  
  
  ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol =.000000001, time.period = MBS.CF.Table[,3], 
                cashflow = MBS.CF.Table[,14], principal = principal, price = price, accrued.interest = accrued.interest)$root
  
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency
  
  #Step7 Present value of the cash flows Present Value Factors
  MBS.CF.Table[,15] = 1/((1+(Yield.To.Maturity/frequency))^(MBS.CF.Table[,3] * frequency))
  
  #Present Value of the cash flows
  MBS.CF.Table[,16] = MBS.CF.Table[,14] * MBS.CF.Table[,15]
  
  #Step8 Risk measures Duration Factors
  MBS.CF.Table[,17] = MBS.CF.Table[,3] * (MBS.CF.Table[,16]/((principal * price) + accrued.interest))
  
  #Convexity Factors
  MBS.CF.Table[,18] = MBS.CF.Table[,3] *(MBS.CF.Table[,3] + 1)
  MBS.CF.Table[,19] = (MBS.CF.Table[,14]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((MBS.CF.Table[,3] + 2) * frequency)))/ 
    ((principal * price) + accrued.interest)
  MBS.CF.Table[,20] = MBS.CF.Table[,18] * MBS.CF.Table[,19] 
  
  #Weighted Average Life
  WAL = sum((((MBS.CF.Table[,5]) + (MBS.CF.Table[,7])) * MBS.CF.Table[,3])/ sum((MBS.CF.Table[,7]) + (MBS.CF.Table[,5])))
  
  #Duration and Convexity
  Duration = apply(MBS.CF.Table, 2, sum)[17]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(MBS.CF.Table, 2, sum)[20] * .5
  
  #Create Class Mortgage Loan Cashflows
  new("MortgageCashFlows",
      bond.id,
      Price = price * 100,
      Accrued = accrued.interest,
      YieldToMaturity = Yield.To.Maturity,
      WAL = WAL,
      ModDuration = Modified.Duration,
      Convexity = Convexity,
      Period = MBS.CF.Table[,1],
      PmtDate = as.character(as.Date(MBS.CF.Table[,2], origin = "1970-01-01")),
      TimePeriod = MBS.CF.Table[,3],
      BeginningBal = MBS.CF.Table[,4],
      MonthlyPmt = MBS.CF.Table[,5],
      MonthlyInterest = MBS.CF.Table[,6],
      PassThroughInterest = MBS.CF.Table[,13],
      ScheduledPrin = MBS.CF.Table[,7],
      PrepaidPrin = MBS.CF.Table[,8],
      EndingBal = MBS.CF.Table[,9],
      ServicingIncome = MBS.CF.Table[,10],
      PMIPremium = MBS.CF.Table[,11],    
      GFeePremium = MBS.CF.Table[,12],
      TotalCashFlow = MBS.CF.Table[,14]
  )
  }  
  #-------------------------------------
  #Risk measures.  These functions measure effective duration, effective convexity, and key rate duration
  #These need to be fixed!!
  #-------------------------------------
  Effective.Duration <- function(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds){
  Price = proceeds/10
  Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)  
  (Price.UP - Price.DWN)/(2*Price*Rate.Delta)
  }

  Effective.Convexity <- function(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds){
  Price = proceeds/10
  Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
  
  (Price.UP + Price.DWN + (2*Price))/(2*(Price*Rate.Delta)^2)
  }  

  #-----------------------------------
  #Bond Term Structure measures key rate duration and spot spread of a standard non callable bond
  # and mortgage cash flow security   
  #-----------------------------------    
  BondTermStructure <- function(bond.id = "character", Rate.Delta = numeric(), TermStructure = "character", principal = numeric(), price = numeric(), cashflow = "character"){
  
  #Call the bond frequency to adjust the spot spread to the payment frequency of the bond
  frequency = bond.id@Frequency
  maturity = bond.id@Maturity
  accrued = cashflow@Accrued
  
  #Class name variable.  This will set the class name for the new class to be initilized
  ClassName <- if(bond.id@BondType != "MBS") {as.character("BondTermStructure")} else {as.character("MortgageTermStructure")}
  
  #Error Trap the user's price input
  if(price <= 1) {price = price} else {price = price/100}
  if(price <=0) stop("No valid bond price")
  proceeds = (principal * price) + accrued 
  
  #========== Set the functions that will be used ==========
  # These functions are set as internal functions to key rates
  # this insures that stored values will not be wrongly be passed to the funtion
  #internal functions used to compute key rate duration and convexity
  Effective.Duration <- function(rate.delta, cashflow, discount.rates, 
                                 discount.rates.up, discount.rates.dwn, t.period, proceeds){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
    (Price.UP - Price.DWN)/(2*proceeds*rate.delta)
  }
  Effective.Convexity <- function(rate.delta, cashflow, discount.rates, 
                                  discount.rates.up, discount.rates.dwn, t.period, proceeds){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
    (Price.UP + Price.DWN - (2*proceeds))/(2*proceeds *(rate.delta^2))
  }
  
  #The spot spread function is used to solve for the spread to the spot curve to normalize discounting
  Spot.Spread <- function(spread, cashflow, discount.rates, t.period, proceeds){
    Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
    proceeds - Present.Value
  }
  
  #================= set up the index names for each array that will be used in the function
  # Index names set names for columns in the KRIndex. This table set the control strucutre for 
  # the loop that will compute key rate duration given rates in the key rate table
  Index.Names <- c("Period", "Time", "Spot Curve", "Disc Curve", "KRDwn", "KRUp")
  
  # KR.Duration.Col set the column names for the table that hold the key rate results
  # this table will output to class bond analytics slot KRTenor and KRDuration
  KR.Duration.Col <- c("Key Rate", "Key Rate Duration", "Key Rate Convexity")
  
  #sets the tenor of the key rate that will report a duration
  KR.Duration.Row <- c("0.25", "1", "2", "3", "5", "7", "10", "15", "20", "25", "30")
  
  #set the arrays for key rate duration calculation
  #key rate table holds data for the term structure and shifts in the key rates
  Key.Rate.Table <- array(data = NA, c(360,6), dimnames = list(seq(c(1:360)), Index.Names))
  
  #key rate duration array holds the key rates and the key rate duration
  KR.Duration <- array(data = NA, c(11,3), dimnames = list(seq(c(1:11)), KR.Duration.Col))
  KR.Duration[,1] <- as.numeric(KR.Duration.Row)
  
  # Create Index for Key Rate Table for interpolation of Key Rate Duration set outer knot points
  # the outer points are the first and last elements in KR string
  # this needs some logic to change the right knot point if the maturity or last payment of the 
  # bond is greater than 30-years should be adaptive
  KR <- c("0.083", "0.25", "1", "2", "3", "5", "7", "10", "15", "20", "25", "30", "30")   # Key Rates
  KRCount = length(KR)
  KRIndex <- array(data = NA, c(KRCount, 6), dimnames = list(seq(c(1:KRCount)), Index.Names))
  
  # Initialize the cash flow array for discounting and key rate caclulations
  # this will be populated from class BondCashFlows 
  CashFlowArray <- array(data = NA, c(360,2), dimnames = list(seq(1:360), c("period", "cashflow")))
  
  #Initialze the spot rate array for key rate duration calculations
  SpotRate <- as.matrix(TermStructure@spotrate)
  
  # Populate Period, Time(t) and Spot Rate Curve of Key Rate Table using NS coefficients from Term Stucture
  # and then populate and align the cashflow array for discounting and key rate computations
  for(x in 1:360){
    
    #Period (n) in which the cashflow is received
    Key.Rate.Table[x,1] = x
    
    # Time (t) at which the cashflow is received
    #Time period in which the cashflow was received for discounting
    Key.Rate.Table [x,2] = x/12
    
    #spot rates for discounting
    Key.Rate.Table[x,3] = SpotRate[x,1]/100   
    
    #Align Cash Flows and populated the CashFlowArray
    #Step One: Make sure all cash flows are set to zero
    CashFlowArray[x,1] = Key.Rate.Table[x,2]
    CashFlowArray[x,2] = 0
  }
  
  #Step Two: Initialize loop and set the cashflows in the array
  #This loops through the time period and set the cashflows into the propery array location for
  #discounts by indexing the cashflows to the array.  The indexing is conditional on the integer of the first period less than or equal to 1
  
  
  if(as.integer(cashflow@TimePeriod[1] *12) != 1) CashFlowArray[as.integer(cashflow@TimePeriod * 12) + 1,2] = cashflow@TotalCashFlow
  if(as.integer(cashflow@TimePeriod[1] * 12) == 1) CashFlowArray[as.integer(cashflow@TimePeriod * 12),2] = cashflow@TotalCashFlow
  
  #solve for spread to spot curve to equal price
  spot.spread <- uniroot(Spot.Spread, interval = c(-1, 1), tol = .0000000001, CashFlowArray[,2],
                         discount.rates = Key.Rate.Table[,3], t.period = Key.Rate.Table[,2] , proceeds)$root
  
  #convert the spot spread to the frequency of the bond
  #spot.spread = (((1+spot.spread)^(1/frequency))-1) * frequency
  #Step three add the spot spread to the spot curve to get the discount rates that are need for
  #the key rate duration calculation
  for(i in 1:360){
    Key.Rate.Table[i,4] = Key.Rate.Table[i,3] + spot.spread                                  
  }
  
  #========= Populate KRIndex Table =========================
  # The key rate index table will serve as the control table for the looping
  # using this table allows for incremental looping of discontinous segments of the
  #spot rate curve and is proprietary to bondlab
  # Step 1 populate Period (n)
  KRIndex[1:KRCount,1] <- round(as.numeric(KR) *12,0)
  
  # Step 2 populate time period (t)
  KRIndex[1:KRCount,2] <- as.numeric(KR)                    
  
  # Step 3 Populate Index Table with the relevant points on the spot curve
  # this is done by looping through the key rate table and allows for term structure implementation
  # other than Nelson Siegel (note: this capability needs to built into bondlab)
  # the key rate index table (KRIndex) is used to populate the key rate table (KRTable)
  for (j in 1:KRCount){                                   
    for (i in 1:360){
      if (Key.Rate.Table[i,1] == round(KRIndex[j,2] * 12,0)) {KRIndex[j,3] = Key.Rate.Table[i,3]} else {KRIndex[j,3] = KRIndex[j,3]}
    }
  }
  
  # Step 4 Populate KRIndex Table with the appropriate Discount Curve values from the key rate table 
  # these will be the reference points for the appropriate key rate shifts
  for (j in 1:KRCount){                                   
    for (i in 1:360){
      if (Key.Rate.Table[i,1] == round(KRIndex[j,2] * 12,0)) {KRIndex[j,4] = Key.Rate.Table[i,4]} else {KRIndex[j,3] = KRIndex[j,3]}
    }
  }
  
  # Step 5 Populated KRIndex Table with KR Shifts
  for (j in 1:KRCount){
    KRIndex[j,5] = KRIndex[j,4] - (Rate.Delta/100)
    KRIndex[j,6] = KRIndex[j,4] + (Rate.Delta/100)
  }
  
  #===== Implement Shift of Spot Rates =======================
  # Once the KRIndex is populated implement the shift in the spot rates using the KRIndex table as the control 
  # w is the counter of the internal knot points used to compute key rate duration it ignores the boundary knots
  # used for interpolation at the end points.  x is the length of the array.  Currently the analysis is limited
  # to loans (bonds) with a maximum of 30-years to maturity.  This can be made dynamic at some point in the future
  # y is column counter used the key rate down and key rate up values
  for (w in 2:(KRCount-1)){ 
    for (x in 1:360){
      for(y in 5:6){
        
        # step 1: populate the spot curve outside the key rate shift =========
        if(Key.Rate.Table[x,2] <= KRIndex[w-1,2] || Key.Rate.Table[x,2] >= KRIndex[w+1,2]) 
        {Key.Rate.Table[x,y] = Key.Rate.Table[x,4]} else {Key.Rate.Table[x,y] = 0}
      }
    }
    
    #===== Begin Interpolation of Spot Curve ==================================
    # Maturity points on the spot rate curve to interpolate
    KRx <- c(KRIndex[w-1,2], KRIndex[w,2], KRIndex[w+1,2]) 
    
    # Spot rates that correspond to the maturity points Down and Up
    for(z in 1:2){                                       
      if (z == 1) 
      {KRy <- c(KRIndex[w-1,4], KRIndex[w,5], KRIndex[w+1,4])}         
      else 
      {KRy <- c(KRIndex[w-1,4], KRIndex[w,6], KRIndex[w+1,4])}                                     
      a = KRIndex[w-1,1]+ 1
      b = KRIndex[w+1,1] - 1
      for(h in a : b){
        Key.Rate.Table[h,(z+4)] <- approx(KRx,KRy, Key.Rate.Table[h,2])$y
      } # Loop through Key Rate Table and interpolation
    } # Inner Loop to set interpolation points from KRIndex
    
    # This line sets the end points for disocunting when the 30-year is last point on the curve
    # It is possible to set the endpoints longer using row 12 of the KRIndex but this will have to wait
    # the cash flow table is programmed for MBS
    if (KRIndex[w,2] == 30) {(Key.Rate.Table[x,5] = KRIndex[12,5]) & (Key.Rate.Table[x,6] = KRIndex[12,6])}   
    
    #============================== Calculate Key Rate Duration ============================================
    KR.Duration[w-1,2] <- -Effective.Duration(
      rate.delta = Rate.Delta/100, 
      cashflow = CashFlowArray[,2], 
      discount.rates = Key.Rate.Table[,4], 
      discount.rates.up = Key.Rate.Table[,6],
      discount.rates.dwn = Key.Rate.Table[,5],
      t.period = Key.Rate.Table[,2],
      proceeds = proceeds
    ) 
    KR.Duration[w-1,3] <- Effective.Convexity(
      rate.delta = Rate.Delta/100, 
      cashflow = CashFlowArray[,2], 
      discount.rates = Key.Rate.Table[,4], 
      discount.rates.up = Key.Rate.Table[,6],
      discount.rates.dwn = Key.Rate.Table[,5],
      t.period = Key.Rate.Table[,2],
      proceeds = proceeds
    ) 
  } # Outer Loop around KRIndex
  new(ClassName,
      bond.id,
      SpotSpread = spot.spread * 100,
      EffDuration = sum(KR.Duration[,2]),
      EffConvexity = sum(KR.Duration[,3]),
      KeyRateTenor = KR.Duration[,1],
      KeyRateDuration = KR.Duration[,2],
      KeyRateConvexity = KR.Duration[,3]
      
  )
  } # End the function

  #---------------------------------------------------
  #Term strucutre call term strc 
  #and holds forward and spot rates as slots to class Term Structure 
  #---------------------------------------------------
  TermStructure <- function(rates.data = "character", method = "character"){
  
  #function(trade.date = "character", method = "character")  
  #Error Trap User inputs to the function
  if(missing(rates.data)) stop("missing rates data object")  
  
  #Default to Nelson-Siegel
  if(missing(method)) method = "ns"
  
  #Default to parametric
  if(method == "cs") stop("cubic spline not implemented")
  
  #Check that the user input a valid method
  CheckMethod <- c("ns", "dl", "sv", "asv", "cs")
  if(!method %in% CheckMethod) stop ("Invalid 'method' Value")
  
  #Call the desired curve from rates data folder
  #trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data <- rates.data
  #rates.data = readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
  
  #set the column counter to make cashflows for termstrucutre
  ColCount <- as.numeric(ncol(rates.data))
  Mat.Years <- as.numeric(rates.data[2,2:ColCount])
  
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
  data$ISSUEDATE <- rep(rates.data[1,1],ColCount - 1)
  data$MATURITYDATE <- as.Date(data$ISSUEDATE) %m+% years(Mat.Years)
  data$COUPONRATE <- as.numeric(rates.data[1,2:ColCount])/100
  data$PRICE <- rep(1000, ColCount -1)
  data$ACCRUED <- rep(0, ColCount -1)
  for(j in 1:(ColCount-1)){
    Vector.Length <- as.numeric(round(difftime(data[[3]][j],
                                               data[[2]][j],
                                               units = c("weeks"))/52.25,0))
    Vector.Length = Vector.Length * 2 #This is frequency multiplier should be dynamic ?
    data$CASHFLOWS$ISIN <- append(data$CASHFLOWS$ISIN, rep(data[[1]][j],Vector.Length))
    data$CASHFLOWS$CF <- append(data$CASHFLOWS$CF,as.numeric(c(rep((data[[4]][j]/2),Vector.Length - 1) *1000, (1000+(data$COUPONRATE[j]/2)*1000))))
    data$CASHFLOWS$DATE <- append(data$CASHFLOWS$DATE,seq(as.Date(rates.data[1,1]) %m+% months(6), as.Date(data[[3]][j]), by="6 months"))
  }
  
  #The Loop Ends here and the list is made
  
  data$TODAY <- as.Date(rates.data[1,1])
  TSInput[[as.character(rates.data[1,1])]] <- c(data)
  print(data$TODAY)
  
  #set term strucutre input (TSInput) to class couponbonds
  class(TSInput) <- "couponbonds"
  
  
  #Fit the term structure of interest rates
  
  if(method != "cs") {TSFit <- estim_nss(dataset = TSInput, group = as.character(rates.data[1,1]), matrange = "all", method = method)} else
  {TSFit <- estim_cs(bonddata = TSInput, group = as.character(rates.data[1,1]), matrange = "all", rse = TRUE)}
  
  #Return the coefficient vector to be passed in to the spot and forward rate functions
  #Maybe have the method choosen based on the one that gives the smallest RMSE
  Vector <- switch(method,
                   ns = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1")]),
                   dl = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2")]),
                   sv = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1", "beta3", "tau2")]),
                   asv = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1", "tau2", "tau3")]),
                   #cs = need to figure this out
  )
  
  #Calculate the spot rate curve for
  period <- seq(from = 1, to = 492, by = 1)
  date <- seq(as.Date(rates.data[1,1]) %m+% months(1), as.Date(data[[3]][j]), by="1 months")
  spot.rate.curve <- spotrates(method = method, beta = Vector, m = seq(from = 1/12, to = 492/12, by = 1/12))
  forward.rate.curve <- forwardrates(method = method, beta = Vector, m = seq(from = 1/12, to = 492/12, by = 1/12))
  Two.Year.Fwd <- (((1 + spot.rate.curve[seq(from = 25, to = 385, by = 1)]) ^ (period[seq(from = 25, to = 385, by = 1)]/12) /
                  (1 + spot.rate.curve[seq(from = 1, to = 361, by = 1)]) ^ (period[seq(from = 1, to = 361, by = 1)]/12))^(1/2))-1
  
  Ten.Year.Fwd <- (((1 + spot.rate.curve[seq(from = 121, to = 481, by = 1)]) ^ (period[seq(from = 121, to = 481, by = 1)]/12) /
                      (1 + spot.rate.curve[seq(from = 1, to = 361, by = 1)]) ^ (period[seq(from = 1, to = 361, by = 1)]/12))^(1/10))-1
  
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
  
  #----------------------------------
  #Prepayment Model Functions.  These functions are used to build the base prepayment model
  #----------------------------------
  
  #----------------------------------
  # Seasoning function is a 3-parameter asymtote exponential function where
  # The three parameter asymptote is equivalent to the PPC ramp
  # a is the asymptote of the function
  # b is the intercept of the function
  # c is the point where the max CPR is achieved
  
  Seasoning <- function(alpha = numeric(), beta = numeric(), theta = numeric(), LoanAge = numeric()){
    if (missing(alpha))
      stop("Need to specify alpha tuning parameter.")
    if (!is.numeric(alpha)  )
      stop("No numeric alpha specified.")
    
    if (missing(beta))
      stop("Need to specify beta tuning parameter.")
    if (!is.numeric(beta)  )
      stop("No numeric beta specified.")
    
    if (missing(theta))
      stop("Need to specify theta tuning parameter.")
    if (!is.numeric(theta)  )
      stop("No numeric theta specified.")
    
    if (missing(LoanAge))
      stop("Need to specify theta tuning parameter.")
    if (!is.numeric(LoanAge)  )
      stop("No numeric theta specified.")
    
    alpha - beta * exp(-theta * LoanAge)}
  
  #----------------------------------
  # Seasonality is modeled as a sin wave
  # a is the amplitude of the wave an set the maximum seasonal factor
  # Month is the calendar month (1..., 12) numeric
  # b is a location parameter shifts the peak values > 1 shift left values < 1 shift right
  Seasonality <- function( alpha = numeric(), Month = numeric(), theta= numeric()){
    
    if (missing(alpha))
      stop("Need to specify alpha tuning parameter.")
    if (!is.numeric(alpha)  )
      stop("No numeric alpha specified.")
    
    if (missing(Month))
      stop("Need to specify Month variable.")
    if (!is.numeric(Month)  )
      stop("No numeric alpha specified.")
    
    if (missing(theta))
      stop("Need to specify Month variable.")
    if (!is.numeric(theta)  )
      stop("No numeric alpha specified.")
    
    (1  + alpha *sin((pi/2 * (Month + theta - 3)) / 3 - 1))}
  
  #-------------------------------------
  # arctanget function with a location parameter
  Borrower.Incentive <- function(incentive = numeric(), theta1 = numeric(), theta2 = numeric(), beta = numeric(), location = numeric()) { 
    theta1 + theta2 * atan(incentive + pi * (beta * ((location - atan(incentive))/pi))) 
  }
  
  #-----------------------------------
  # Burnout is an exponentially decreasing function
  # a is the coefficient on the burnout varaible and b is the measure of burnout
  Burnout <- function(beta1 = numeric(), beta2= numeric(), MaxIncen = numeric(), LoanAge = numeric()){
    exp(beta1 * LoanAge +  beta2 * MaxIncen)
  }  

  # The Bond Lab base prepayment model
  #----------------------------------------------------------------------------------------------------
  Prepayment.Model <- function(ModelTune = "character", LoanAge = numeric(), 
                               Month = numeric(), incentive = numeric(), Burnout.maxincen = numeric()){
      
      TurnoverRate        = ModelTune@TurnoverRate                         
      Seasoning.alpha     = ModelTune@Turnover.alpha
      Seasoning.beta      = ModelTune@Turnover.beta 
      Seasoning.theta     = ModelTune@Turnover.theta
      Seasonality.alpha   = ModelTune@Seasonality.alpha
      Seasonality.theta   = ModelTune@Seasonality.theta
      Fast.theta1         = ModelTune@Incentive.Fast.theta.1  
      Fast.theta2         = ModelTune@Incentive.Fast.theta.1 
      Fast.beta           = ModelTune@Incentive.Fast.beta 
      Fast.location       = ModelTune@Incentive.Fast.eta
      Slow.theta1         = ModelTune@Incentive.Slow.theta.1 
      Slow.theta2         = ModelTune@Incentive.Slow.theta.2 
      Slow.beta           = ModelTune@Incentive.Slow.beta 
      Slow.location       = ModelTune@Incentive.Slow.eta
      Burnout.beta1       = ModelTune@Burnout.beta.1 
      Burnout.beta2       = ModelTune@Burnout.beta.2
    
    # All of the above needs to made into a model tuning class 
    # Restate the turnover rate as a single monthly mortality rate
    
    Turnover.Rate <- 1-(1 - TurnoverRate)^(1/12)
    
    Turnover <- Turnover.Rate * 
      Seasoning(alpha = Seasoning.alpha, beta = Seasoning.beta, theta = Seasoning.theta, LoanAge = LoanAge) *
      Seasonality(alpha = Seasonality.alpha, Seasonality.theta, Month = Month)
    
    # Calculate the Borrower Refinance Response
    Fast <- Borrower.Incentive(incentive = incentive, theta1 = Fast.theta1, theta2 = Fast.theta2, beta = Fast.beta, location = Fast.location)
    Slow <- Borrower.Incentive(incentive = incentive, theta1 = Slow.theta1, theta2 = Slow.theta2, beta = Slow.beta, location = Slow.location)
    Burnout <- Burnout(beta1 = Burnout.beta1, beta2 = Burnout.beta2, MaxIncen = Burnout.maxincen * 100, LoanAge = LoanAge)
    
    Refinance <- (Fast * Burnout) + (Slow * (1-Burnout))
    
    SMM <-pmax(0, Refinance + Turnover)
    
  }
  
   
  # ---------  This function is the prepayment model and serves as a constructor for the prepayment model vector 
  # ---------  Prepayment Assumption
  PrepaymentAssumption <- function(bond.id = "character", TermStructure = "character", ModelTune = "character", Burnout = numeric(),
                                   PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), 
                                   seasoning.period = numeric(), CPR = numeric()){
    #Check for a valid prepayment assumption
    if(!PrepaymentAssumption %in% c("MODEL", "CPR", "PPC")) stop("Not a Valid Prepayment Assumption")
    PrepayAssumption <- PrepaymentAssumption    
    
    #Error Trap the CPR assumption
    if(PrepaymentAssumption == "CPR") if(CPR >=1) {CPR = CPR/100} else {CPR = CPR}
    #PPC function has error trapping feature so there is no need to error trap for PPC
    
    NoteRate = bond.id@GWac
    FirstPmtDate = as.Date(bond.id@FirstPrinPaymentDate, "%m-%d-%Y")
    LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
    FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
    NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
    
    col.names <- c("Period", "PmtDate", "LoanAge", "TwoYearFwd", "TenYearFwd", "MtgRateFwd", "SMM")
    
    Mtg.Term = as.integer(difftime(FinalPmtDate, FirstPmtDate, units = "days")/30.44) +1
    Remain.Term = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/30.44)
    Period = seq(from = 1, to = Remain.Term, by = 1)
    PmtDate = as.Date(NextPmtDate)  %m+% months(seq(from = 0, to = Remain.Term-1, by = 1)) 
    LoanAge = as.integer(difftime(as.Date(NextPmtDate)  %m+% months(seq(from = 1, to = Remain.Term, by = 1)), 
                                  FirstPmtDate, units = "days")/30.44) + 1
    
    NoteRate =  as.numeric(rep(NoteRate, length(LoanAge)))
    Mtg.Rate =  as.numeric(TermStructure@TenYearFwd[1:length(LoanAge)] + .80)
    Incentive =  as.numeric(NoteRate - Mtg.Rate)
    Burnout = Burnout
    BurnoutVector = NULL
    for(i in seq_along(LoanAge)){
    if(i == 1) if(Burnout[i] > Incentive[i]) BurnoutVector[i] = Burnout[i] else BurnoutVector[i] = Incentive[i]
    if(i > 1) if(BurnoutVector[i-1] > Incentive[i]) BurnoutVector[i] = BurnoutVector[i-1] else BurnoutVector[i] = Incentive[i]
    }

      
    if(PrepaymentAssumption == "MODEL")
      {SMM = Prepayment.Model(ModelTune = ModelTune, LoanAge = LoanAge, Month = as.numeric(format(PmtDate, "%m")), 
                              incentive = Incentive, Burnout.maxincen = BurnoutVector)} 
      else 
      {if(PrepaymentAssumption == "PPC") 
      {SMM = as.numeric(1-(1-PPC.Ramp(begin.cpr = begin.cpr, end.cpr = end.cpr, 
                                      season.period = seasoning.period, period = LoanAge))^(1/12))} 
      else
      {SMM = rep(1-(1-CPR)^(1/12), Remain.Term)}
      }
    
    new("PrepaymentAssumption",
        PrepayAssumption = as.character(PrepayAssumption),
        PPCStart = if(PrepaymentAssumption == "PPC") {begin.cpr} else {0},
        PPCEnd = if(PrepaymentAssumption == "PPC") {end.cpr} else {0},
        PPCSeasoning = if(PrepaymentAssumption == "PPC") {seasoning.period} else {0},
        NoteRate = as.numeric(NoteRate),
        FirstPmtDate = as.character(FirstPmtDate),
        LastPmtDate = as.character(LastPmtDate),
        FinalPmtDate = as.character(FinalPmtDate),
        Period = Period,
        PmtDate = as.character(PmtDate),
        LoanAge = as.numeric(LoanAge),
        MtgRateFwd = Mtg.Rate,
        Incentive = Incentive,
        BurnoutVector = BurnoutVector,
        SMM = as.numeric(SMM)
    )
        
  }
  
# ---------------- This function is the dollar roll analysis ---------------------------
# ---------------- Currently the function calcualtes the 1 month roll ------------------
# ---------------- Upgrade the bond basis function for actual/actual day count ---------  
  DollarRoll <- function(bond.id = "character", price = numeric(), drop = numeric(), original.bal = numeric(), 
                         settlement.date = "character", fwd.settlement.date = "character", 
                         reinvestment.rate = numeric(), finance.rate = numeric(),
                         MortgageCashFlow = "character") {
    
    #need to error trap these inputs
    #reinvestment rate, drop
    
    #Error Trap the user's price input
    if(price <= 1) {price = price} else {price = price/100}
    if(price <= 0) stop("No valid bond price")
    
    #Error Trap the user's drop input
    #if(drop < 1/32) {drop = drop} else {drop = drop/100}
    
    #Here upgrade the bond basis function to include actual day count
    settlement.date = as.Date(settlement.date, "%m-%d-%Y")
    fwd.settlement.date = as.Date(fwd.settlement.date, "%m-%d-%Y")
    reinvestment.days = as.numeric(difftime(fwd.settlement.date, settlement.date, units = "days"))
    
    Factor = bond.id@MBSFactor
    CurrentBal = as.numeric(original.bal * Factor)
    BeginningMarketValue = original.bal * Factor * price
    
    IssueDate = as.Date(bond.id@IssueDate)
    DatedDate = as.Date(bond.id@DatedDate)
    Maturity = as.Date(bond.id@Maturity)
    Frequency = as.numeric(bond.id@Frequency)
    Coupon = as.numeric(bond.id@Coupon)
    
    FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
    LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
    NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
    RemainingTerm = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/30.44)
    FwdPrice = price - (drop/100)
    reinvestment.rate = reinvestment.rate 
    
    #Dollar Roll Hold versus Roll Analysis..
    #Dollar Roll Proceeds
    Accrued = MortgageCashFlow@Accrued
    TotalProceeds = BeginningMarketValue + Accrued
    ReinvestmentIncome = as.numeric(TotalProceeds * reinvestment.rate * (reinvestment.days/360))
    TotalRollProceeds = TotalProceeds + ReinvestmentIncome
    
    #Hold Proceeds - Calculate the value of holding the MBS
    ScheduledPrin = as.numeric(MortgageCashFlow@ScheduledPrin[1])
    PrepaidPrin = as.numeric(MortgageCashFlow@PrepaidPrin[1])
    PassThroughInterest = as.numeric(MortgageCashFlow@PassThroughInterest[1])
    RemainingBalance = as.numeric(MortgageCashFlow@EndingBal[1])
    
    #Forward settlement and payment dates to compute the value of holding versus rolling
    #Roll Settlement Dates forward by the roll months.  For example one month roll, two months
    #Default value is one month
    
    FwdNextPmtDate = NextPmtDate %m+% months(1) #The months (1) should be a variable allowing for two-, and three- months
    FwdLastPmtDate = LastPmtDate %m+% months(1)
    days.to.nextpmt = BondBasisConversion(issue.date = IssueDate, start.date = DatedDate, end.date = Maturity,
                                          settlement.date = fwd.settlement.date, lastpmt.date = FwdLastPmtDate, nextpmt.date = FwdNextPmtDate)
    days.to.nextpmt = days.to.nextpmt * 360
    days.between.pmtdate = ((12/Frequency)/12) * 360
    days.of.accrued = (days.between.pmtdate - days.to.nextpmt) 
    
    FutureValueofPmts = ScheduledPrin + PrepaidPrin + PassThroughInterest
    FuturePrincipalProceeds = RemainingBalance * FwdPrice 
    FwdAccrued = (days.of.accrued/days.between.pmtdate) * as.numeric(MortgageCashFlow@MonthlyInterest[2])
    FutureValueHold = FutureValueofPmts + FuturePrincipalProceeds + FwdAccrued
    
    #Compute the Roll Economics....
    #Hold or Roll Analysis and Economics of Trade  
    if (TotalRollProceeds > FutureValueHold) 
    HoldorRoll = as.character("Roll") else HoldorRoll = as.character("Hold")  
    Advantage = as.numeric(abs(TotalRollProceeds - FutureValueHold))
    
    #Compute the days between the roll settlment date and the payment date
    #This is to derive the discounted value of the carry between the settlement date and the payment date
    #The discount rate is applied to the Future Value of the payments and is the Discounted Value of the Carry
    MBSPmtDate = as.Date(MortgageCashFlow@PmtDate[1], "%Y-%m-%d")
    settlement.day.diff = as.integer(difftime(MBSPmtDate, fwd.settlement.date, units = "days"))
    DiscValueofCarry = FutureValueofPmts * ((1 +finance.rate) ^ (settlement.day.diff/360))
    
    FutureValuePrinCarry = (RemainingBalance * price) + FwdAccrued + DiscValueofCarry
    FinanceCost = as.numeric(TotalProceeds * finance.rate * (reinvestment.days/361))
    TotalFutureValue = FutureValuePrinCarry - FinanceCost
    
    DropImpliedValue = ((TotalFutureValue - TotalProceeds)/RemainingBalance) * 32
    
    new("DollarRoll",
        #MBS price and settlment information
        SettlementDate = as.character(settlement.date),
        FwdSettlementDate = as.character(fwd.settlement.date),
        Price = price * 100,
        Drop = drop,
        FwdPrice = FwdPrice * 100,
        # MBS information
        GrossCoupon = bond.id@GWac,
        NetCoupon = bond.id@Coupon,
        OriginalTerm = bond.id@AmortizationTerm,
        RemainingTerm = RemainingTerm,
        OrigBalance = original.bal,
        CurrentBalance = CurrentBal,
        #Settlement information
        PrincipalProceeds = BeginningMarketValue,
        Accrued = Accrued,
        TotalProceeds = TotalProceeds,
        DaysInterest = reinvestment.days,
        ReinvestmentIncome = ReinvestmentIncome,
        # MBS hold information
        ScheduledPrin = ScheduledPrin,
        PrepaidPrin = PrepaidPrin,
        PassThroughInterest = PassThroughInterest,
        FutureValueHold = FutureValueHold,
        RemainingBalance = RemainingBalance,
        FuturePrincipalProceeds = FuturePrincipalProceeds,
        FwdAccrued = FwdAccrued,
        # Investor financing rates
        FinanceRate = finance.rate,
        ReinvestmentRate = reinvestment.rate,
        FutureValueRoll = TotalRollProceeds,
        FutureValuePrinCarry = FutureValuePrinCarry,
        DiscValueofCarry = DiscValueofCarry,
        # Roll analysis
        HoldorRoll = HoldorRoll,
        Advantage = Advantage,
        TotalFutureValue = TotalFutureValue,
        DropImpliedValue = DropImpliedValue,
        MortgageCashFlow
    )
  }
  
# ----------------------------------------------------------
# Scenario Analysis Function
# Runs interest rate scenario analysis based on scenario set
# ----------------------------------------------------------
  Mtg.Scenario <- function(scenario.set = vector(), scenario.type = "character", price = numeric(), 
                       rates.data = "character", method = "character", 
                       bond.id = "character", original.bal = numeric(), settlement.date = "character", 
                       PrepaymentAssumption = "character", 
                       ModelTune = "character", Burnout = numeric(), 
                       begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()){ 
    
    #set the default for Scenario
    if(missing(scenario.type)) scenario.type = "rate"
    
    #should take rate or cpr build in a switch function
    ScenarioResult <- list()
    
    #Normalize the scenario vector to add to the rates data.  
    #Users typically specify rate shifts in basis points but recall rate data
    #in percent from most data sources.  Scenario set is divided by 100
    scenario.set = (scenario.set/100)
    
    #Initialize the first loop to run over scenario set    
    for(i in 1:length(scenario.set)){
      # add the rate shift to rates
      rates = rates.data      
      rates[1,2:length(rates.data)] = as.character(as.numeric(Rates[1,2:length(rates.data)]) + scenario.set[i])
      
      TermStructure = TermStructure(rates.data = rates, method = method)
      
      Prepayment = PrepaymentAssumption(bond.id = bond.id, 
                                        TermStructure = TermStructure, 
                                        PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                                        begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
      
      MortgageCashFlow = MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                                           price = price, PrepaymentAssumption = Prepayment)
      
      Scenario <- new("Mtg.Scenario",
                  Name = paste("Scenario", scenario.set[i] * 100, sep = ""),    
                  Period = MortgageCashFlow@Period,
                  PmtDate = MortgageCashFlow@PmtDate,
                  TimePeriod = MortgageCashFlow@TimePeriod,
                  BeginningBal = MortgageCashFlow@BeginningBal,
                  PassThroughInterest = MortgageCashFlow@PassThroughInterest,
                  ScheduledPrin = MortgageCashFlow@ScheduledPrin,
                  PrepaidPrin = MortgageCashFlow@PrepaidPrin,
                  EndingBal = MortgageCashFlow@EndingBal,
                  TotalCashFlow = MortgageCashFlow@PassThroughInterest + 
                  MortgageCashFlow@ScheduledPrin + 
                  MortgageCashFlow@PrepaidPrin,
                  spotrate = TermStructure@spotrate,
                  forwardrate = TermStructure@forwardrate,
                  SMM = Prepayment@SMM)
      
      ScenarioResult <- append(ScenarioResult, Scenario)
      
    } # end the for loop 
     
     new("Mtg.ScenarioSet", 
         Scenario = ScenarioResult,
         MortgageCashFlow)

  }  # ends the function Scenario
 
# -------------------------------
# Rate of Return Analysis
# -------------------------------
  RateofReturn <- function(ReinvestmentRate = numeric(), ReceivedCF = "character", 
                           RemainingCF = "character", SpotCurve = "character", FwdCurve = "character", HorizonSpread = numeric()) {
    
  }
# --------------------------------
# Bond Analytics Functions - THESE ARE THE BOND LAB ENGINES !!!
# These functions are different from the above they use the functions together
# to analyze a bond or mortgage backed security using the above functions and construct the appropriate objects (classes)  
# --------------------------------

# This function analyzes a standard non callable bond and serves as the constructor function
# These are the engines  
# -----------------------------------
  BondAnalytics <- function (bond.id = "character", principal = numeric(), price = numeric(), trade.date = "character", 
                           settlement.date = "character", method = method) 
{

  #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) method = "ns"
  
  #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
  Rate.Delta = 1
  
  # The first steo is to read in the Bond Detail
  bond.id <- readRDS(paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""))
  #Call the desired curve from rates data folder
  trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data <- readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
  
  #The first step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)

  #The second step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
  #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
  BondCashFlow <- BondCashFlows(bond.id = bond.id, principal = principal, settlement.date = settlement.date, price = price)
  
  #The third step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  BondTermStructure <- BondTermStructure(bond.id = BondCashFlow, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
                                         principal = principal, price = price, cashflow = BondCashFlow)
  
  new("BondAnalytics", bond.id, BondCashFlow, BondTermStructure, TermStructure)
  
  }


  # This function analyzes a standard pass through security and serves as the constructor function
  #--------------------------------------  
  PassThroughAnalytics <- function (bond.id = "character", original.bal = numeric(), price = numeric(), trade.date = "character", 
                                  settlement.date = "character", method = "character", scenario.set = vector(),
                                  PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()) 
  {
  
  #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) method = "ns"
  
  #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
  Rate.Delta = 1
  
  # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
  bond.id <- readRDS(paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""))
  #Call the desired curve from rates data folder
  trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data <- readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
  #Call Prepayment Model
  ModelTune <- readRDS(paste("~/BondLab/PrepaymentModel/",bond.id@Model,".rds", sep = ""))
  Burnout = bond.id@Burnout
 
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  #Third if mortgage security call the prepayment model
  PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, TermStructure = TermStructure, 
  PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
  begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
    
  #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
  #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
  MortgageCashFlow <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                      price = price, PrepaymentAssumption = PrepaymentAssumption)
  
  #The fifth step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  MortgageTermStructure <- BondTermStructure(bond.id = MortgageCashFlow, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
                          principal = original.bal *  MortgageCashFlow@MBSFactor, price = price, cashflow = MortgageCashFlow)
  
  Scenario <- Mtg.Scenario(scenario.set = scenario.set, price = price, rates.data = rates.data, 
                       method = method, bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                       PrepaymentAssumption = "MODEL",
                       begin.cpr = begin.cpr, end.cpr = begin.cpr, seasoning.period = seasoning.period, 
                       CPR = CPR, ModelTune = ModelTune, Burnout = Burnout)     
  
  new("PassThroughAnalytics", bond.id, MortgageCashFlow, MortgageTermStructure, TermStructure, PrepaymentAssumption, Scenario)    
  }
   
  #----------------------------------
  #Agency Mortgage Dollar Roll
    
    DollarRollAnalytics <- function(bond.id = "character", original.bal= numeric(), price = numeric(), drop = numeric(), trade.date = "character", 
                           settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(),  
                           finance.rate = numeric(), method = "ns", PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()) {
    
    #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
    #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
    if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
    
    #Default method for TermStructure
    if(missing(method)) method = "ns"
    
    #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
    Rate.Delta = 1
    
    # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
    bond.id <- readRDS(paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""))
    #Call the desired curve from rates data folder
    trade.date = as.Date(trade.date, "%m-%d-%Y")
    rates.data <- readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
    #Call Prepayment Model
    ModelTune <- readRDS(paste("~/BondLab/PrepaymentModel/",bond.id@Model,".rds", sep = ""))
    Burnout = bond.id@Burnout
   
    #The second step is to call the desired coupon curve into memory 
    #This is done with the TermStructure function which creates the class TermStructure
    TermStructure <- TermStructure(rates.data = rates.data, method = method)
    
    # Third if mortgage security call the prepayment model
    PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, TermStructure = TermStructure, 
                                                 PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                                                 begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
    
    #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
    #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
    MortgageCashFlow <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                                          price = price, PrepaymentAssumption = PrepaymentAssumption)
    
   
    DollarRoll <- DollarRoll(bond.id = bond.id, price = price, drop = drop, original.bal = original.bal, 
                             settlement.date = settlement.date, fwd.settlement.date = fwd.settlement.date, 
                             reinvestment.rate = reinvestment.rate, finance.rate = finance.rate, MortgageCashFlow = MortgageCashFlow)
   return(DollarRoll)
  }
  
  #-------------- Scenario Analysis
   Mtg.ScenarioAnalysis <- 
    function( scenario.set = vector(), scenario.type = "character", bond.id = "character", original.bal= numeric(), 
              price = numeric(), trade.date = "character", settlement.date = "character", method = "character", 
              PrepaymentAssumption = "character", ..., 
              begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()) {
    
    #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
    #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
    if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
    
    #Default for scenario.type
    if(missing(scenario.type)) scenario.type = "rate"
    
    #Default method for TermStructure
    if(missing(method)) method = "ns"
    
    #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
    Rate.Delta = 1
    
    # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
    bond.id <- readRDS(paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""))
    #Call the desired curve from rates data folder
    trade.date = as.Date(trade.date, "%m-%d-%Y")
    rates.data <- readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
    #Call Prepayment Model
    ModelTune <- readRDS(paste("~/BondLab/PrepaymentModel/",bond.id@Model,".rds", sep = ""))
    Burnout = bond.id@Burnout
    
    # This is call to the scenario function it is not part of the scenario function stupid!!
    Scenario <- Mtg.Scenario(scenario.set = scenario.set, price = price, rates.data = rates.data, 
                         method = method, bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                         PrepaymentAssumption = PrepaymentAssumption, ..., 
                         begin.cpr = begin.cpr, end.cpr = begin.cpr, seasoning.period = seasoning.period, 
                         CPR = CPR, ModelTune = ModelTune, Burnout = Burnout)
    
    return(Scenario)
  }
  
  
  # This is bondlab!! the final call to the analytic engines ...
  
  BondLab  <- function (bond.id = "character", principal = numeric(), price = numeric(), trade.date = "character", 
                        settlement.date = "character", method = method, scenario.set = vector(), ...,
                        PrepaymentAssumption = "character", 
                        begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()){
    
  }
  
  #----------------------------------
  # Helper Functions These function help to manage
  # The data sources   
  #----------------------------------

  # Swap Curve data creates a data base of daily yield
  # curves using swap rate data from the Federal Reserve

  SwapRateData <- function(datafile = "character", maturityvector = numeric()){
  #========== Read Swap Rate Data ===========================
  SwapRateData <-read.csv(datafile, header = TRUE, as.is = TRUE)
  #======== remove month and year data and reorder dataset
  RowCount = nrow(SwapRateData)
  ColCount = ncol(SwapRateData)
  
  for(i in 1:RowCount) {
    if(SwapRateData[i,ColCount] != "ND") {data = SwapRateData[i,]                                      
                                          data <- rbind(data, as.numeric(maturityvector))
                                          saveRDS(data, paste(data[1,1], ".rds", sep = ""), compress = TRUE)}}
  }

  # Multiple plot function
  # Source: cookbook for R
  # Author: Winston Change
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.

  multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      }
    }
  }


  #-----------------------
  # Classes these are the new classes used by by Bond Lab
  #------------------------

  # --- The following classes define standard bond analytics
  setClass("BondDetails",
         representation(
           Cusip = "character",
           ID = "character",
           BondType = "character",
           Sector ="character",
           Coupon = "numeric",
           IssueDate = "character",
           DatedDate = "character",
           Maturity = "character",
           LastPmtDate = "character",
           NextPmtDate = "character",
           Moody = "character",
           SP = "character",
           BondLab  = "character",
           Frequency = "numeric",
           BondBasis = "character",
           Callable = "character",
           Putable = "character",
           SinkingFund = "character"))

  setClass("BondCashFlows",
         representation(
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
           TotalCashFlow = "numeric"),
         contains = "BondDetails")

  setClass("BondTermStructure",
         representation(
           SpotSpread = "numeric",   
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric"),
         contains = "BondDetails")

# --- The folllowing classes define standard Mortgage Passthrough analytics
  setClass("MBSDetails", 
         representation(
           Cusip = "character",
           ID = "character",
           BondType = "character",
           Sector ="character",
           Coupon = "numeric",
           IssueDate = "character",
           DatedDate = "character",
           Maturity = "character",
           LastPmtDate = "character",
           NextPmtDate = "character",
           PaymentDelay = "numeric",
           Moody = "character",
           SP = "character",
           BondLab  = "character",
           Frequency = "numeric",
           BondBasis = "character",
           GWac = "numeric",
           AmortizationType = "character",
           AmortizationTerm = "numeric",
           Index = "character",
           Margin = "numeric",
           FirstPmtDate = "character",
           FinalPmtDate = "character",
           Servicing = "numeric",
           PMI = "numeric",
           Gfee = "numeric",
           InitialInterest = "character",
           InterestOnlyPeriod = "numeric",
           FirstPrinPaymentDate = "character",
           BalloonPmt = "character",
           BalloonDate = "character",
           MBSFactor = "numeric",
           Model = "character",
           Burnout = "numeric"
         ))
  
  setClass("PrepaymentAssumption",
           representation(
             PrepayAssumption = "character",
             PPCStart = "numeric",
             PPCEnd = "numeric",
             PPCSeasoning = "numeric",
             FirstPmtDate = "character",
             LastPmtDate = "character",
             FinalPmtDate = "character",
             PmtDate = "character",
             LoanAge = "numeric",
             Period = "numeric",
             NoteRate = "numeric",
             MtgRateFwd = "numeric",
             Incentive = "numeric",
             BurnoutVector = "numeric",
             SMM = "numeric"),
             contains = "MBSDetails"
           )

  setClass("MortgageCashFlows",
         representation(
           Price = "numeric",
           Accrued = "numeric",
           YieldToMaturity = "numeric",
           WAL = "numeric",
           ModDuration = "numeric",
           Convexity = "numeric",
           Period = "numeric",
           PmtDate = "character",
           TimePeriod = "numeric",
           BeginningBal = "numeric",
           MonthlyPmt = "numeric",
           MonthlyInterest = "numeric",
           PassThroughInterest = "numeric",
           ScheduledPrin = "numeric",
           #SMM = "numeric",
           PrepaidPrin = "numeric",
           EndingBal = "numeric",
           ServicingIncome = "numeric",
           PMIPremium = "numeric",
           GFeePremium = "numeric",  
           TotalCashFlow = "numeric"),
           contains = "MBSDetails"
         )

  setClass("MortgageTermStructure",
         representation(
           SpotSpread = "numeric",   
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric"),
         contains = "MBSDetails"
         )

  setClass("DollarRoll",
          representation(
           SettlementDate = "character",
           FwdSettlementDate = "character",  
           GrossCoupon = "numeric",
           NetCoupon = "numeric",
           OriginalTerm = "numeric",
           RemainingTerm = "numeric",
           OrigBalance = "numeric",
           CurrentBalance = "numeric",
           Price = "numeric",
           PrincipalProceeds = "numeric",
           Accrued = "numeric",
           TotalProceeds = "numeric",
           DaysInterest = "numeric",
           ReinvestmentIncome = "numeric",
           ScheduledPrin = "numeric",
           PrepaidPrin = "numeric",
           PassThroughInterest = "numeric",
           FutureValueHold = "numeric",
           RemainingBalance = "numeric",
           FuturePrincipalProceeds = "numeric",
           FwdAccrued = "numeric",
           Drop = "numeric",
           FwdPrice = "numeric",
           FinanceRate = "numeric",
           ReinvestmentRate = "numeric",
           HoldorRoll = "character",
           Advantage = "numeric",
           FutureValueRoll = "numeric",
           DiscValueofCarry = "numeric",
           FutureValuePrinCarry = "numeric",
           TotalFutureValue = "numeric",
           DropImpliedValue = "numeric"),
           contains = "MortgageCashFlows"
        ) 

# --- The following classes define rates and Prepayment model tune classes
# --- these classes are used to pass term strucuture information and prepayment model
# --- tuning paramaters  

  setClass("TermStructure",
         representation(
           tradedate = "character",
           period = "numeric",
           date = "character",
           spotrate = "numeric",
           forwardrate = "numeric",
           TwoYearFwd = "numeric",
           TenYearFwd = "numeric"
           ))

  setClass("PrepaymentModelTune",
        representation(
          TurnoverRate = "numeric",
          Turnover.alpha = "numeric",
          Turnover.beta = "numeric",
          Turnover.theta = "numeric",
          Seasonality.alpha = "numeric",
          Seasonality.theta = "numeric",
          Incentive.Fast.theta.1 = "numeric",
          Incentive.Fast.theta.2 = "numeric",
          Incentive.Fast.beta = "numeric",
          Incentive.Fast.eta = "numeric",
          Incentive.Slow.theta.1 = "numeric",
          Incentive.Slow.theta.2 = "numeric",
          Incentive.Slow.beta = "numeric",
          Incentive.Slow.eta = "numeric",
          Burnout.beta.1 = "numeric",
          Burnout.beta.2 = "numeric"
          ))
  
# ----- The following classes define rate of return and valuation classes
  setClass("Mtg.Scenario",
           representation(
           Name = "character",   
           Period = "numeric",
           PmtDate = "character",
           TimePeriod = "numeric",
           BeginningBal = "numeric",
           PassThroughInterest = "numeric",
           ScheduledPrin = "numeric",
           PrepaidPrin = "numeric",
           EndingBal = "numeric",
           TotalCashFlow = "numeric",
           spotrate = "numeric",
           forwardrate = "numeric",
           SMM = "numeric"
             ))

  setClass("Mtg.ScenarioSet",
             representation(
               Scenario = "list"),
               contains = "MortgageCashFlows"
             )
        
  setClass("RateofReturn",
         representation(
         PmtDate = "character",
         Period = "numeric",
         ReinvestmentRate = "numeric",
         ReceivedCF = "numeric",
         ReInvestmentIncome = "numeric",
         RemainingCF = "numeric",
         HorizonSpread = "numeric"))

#------ The classes BondCashFlows and BondTermStructure extends the BondAnalytics a single storage class for all bond analytics
  setClass("BondAnalytics", contains = c("MBSDetails", "BondCashFlows", "BondTermStructure", "TermStructure"))

#------ The classes MortgageCashFlows and Mortgage TermStructure extends the MortgageAnalytics a single storage class 
#------ for all mortgage passthrough analytics

  setClass("PassThroughAnalytics", 
           contains = c("MBSDetails", "MortgageCashFlows", "MortgageTermStructure", "TermStructure", "PrepaymentAssumption", "Mtg.ScenarioSet"))


#---------------------------------------
# Bond Lab Initialize Set Generics
#---------------------------------------
  setGeneric(
  name = "BondCashFlows",
  def = function (bond.id = "character", principal = numeric(), settlement.date = "character", price = numeric())
  {standardGeneric("BondCashFlows")})

  setGeneric(
  name = "MortgageCashFlows",
  def = function(bond.id = "character", original.bal = numeric(), settlement.date = "character", 
                 price = numeric(), PrepaymentAssumption = "character")
  {standardGeneric("MortgageCashFlows")})

  setGeneric("BondTermStructure",
           def = function(bond.id = "character", Rate.Delta = numeric(), TermStructure = "character", principal = numeric(), 
                          price = numeric(), cashflow = "character")
           {standardGeneric("BondTermStructure")})

  setGeneric("BondAnalytics",
           def = function (bond.id = "character", principal = numeric(), price = numeric(), trade.date = "character", 
                           settlement.date = "character", method = method)
           {standardGeneric("BondAnalytics")})

  setGeneric("TermStructure",
           function(rates.data = "character", method = "character")
           {standardGeneric("TermStructure")})

  setGeneric("PassThroughAnalytics",
           function (bond.id = "character", original.bal = numeric(), price = numeric(), trade.date = "character", 
                     settlement.date = "character", method = method, scenario.set = vector(), PrepaymentAssumption = "character",
                     ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
             {standardGeneric("PassThroughAnalytics")})
  
  setGeneric("PrepaymentAssumption",
           function(bond.id = "character", TermStructure = "character", ModelTune = "character", Burnout = numeric(),
                    PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), 
                    seasoning.period = numeric(), CPR = numeric())
           {standardGeneric("PrepaymentAssumption")}
           )  

  setGeneric("RateofReturn",
           function(ReinvestmentRate = numeric(), ReceivedCF = "character", RemainingCF = "character",
                    SpotCurve = "character", FwdCurve = "character", HorizonSpread = numeric())
             {standardGenric("RateofReturn")})

  setGeneric("Seasoning",
           function (alpha = numeric(), beta = numeric (), theta = numeric(), LoanAge = numeric())
             {standardGeneric("Seasoning")})

  setGeneric("Borrower.Incentive",
           function(incentive = numeric(), theta1 = numeric(), theta2 = numeric(), beta = numeric(), location = numeric())
             {standardGeneric("Borrower.Incentive")})

  setGeneric("Burnout",
           function(beta1 = numeric(), beta2 = numeric(), MaxIncen = numeric(), LoanAge = numeric())
             {standardGeneric("Burnout")})

  setGeneric("Seasonality",
           function(alpha = numeric(), Month = numeric(), theta = numeric())
             {standardGeneric("Seasonality")})
  
  setGeneric("Prepayment.Model",
           function(ModelTune = "character", LoanAge = numeric(), 
                    Month = numeric(), incentive = numeric(), Burnout.maxincen = numeric())
             {standardGeneric("Prepayment.Model")})  

  setGeneric("DollarRoll", function(bond.id = "character", price = numeric(), drop = numeric(), original.bal = numeric(), 
                         settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(), finance.rate = numeric(), MortgageCashFlow = "character")
            {standardGeneric("DollarRoll")})
  
  setGeneric("DollarRollAnalytics", function(bond.id = "character", original.bal= numeric(), price = numeric(), drop = numeric(), trade.date = "character", 
                        settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(), finance.rate = numeric(), method = "ns", 
                        PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
            {standardGeneric("DollarRollAnalytics")})

  setGeneric("Mtg.Scenario", function(scenario.set = vector(), scenario.type = "character", price = numeric(), rates.data = "character",method = "character", 
                       bond.id = "character", original.bal = numeric(), settlement.date = "character", 
                       PrepaymentAssumption = "character", 
                       ModelTune = "character", Burnout = numeric(), 
                       begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
            {standardGeneric("Mtg.Scenario")})

  setGeneric("Mtg.ScenarioAnalysis", function( scenario.set = vector(), scenario.type = "character", bond.id = "character", original.bal= numeric(), 
                      price = numeric(), trade.date = "character", settlement.date = "character", method = "character", 
                      PrepaymentAssumption = "character", ..., 
                      begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
              {standardGeneric("Mtg.ScenarioAnalysis")})  
  
#-------------------------------
#Bond Lab Set Methods 
#-------------------------------
setMethod("show",
          signature(object = "BondCashFlows"),
          function (object) 
          {      
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            plotdata = as.data.frame(cbind(object@Period, object@TotalCashFlow))
            colnames(plotdata) <- c("Period", "CashFlow")
            
            plot <- ggplot(plotdata, aes(x= Period, y = CashFlow)) +
              geom_bar(stat = "identity", fill = "Grey") +
              theme_minimal() + 
              labs(fill = "") +
              ylab("Bond Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
                         
            print(plot)

          }
)

setMethod("show",
          signature(object = "MortgageCashFlows"),
          function (object) 
          {      
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            
            plotdata = as.data.frame(cbind(object@Period, object@ScheduledPrin, object@PrepaidPrin, 
                          object@PassThroughInterest, object@ServicingIncome, object@PMIPremium, object@GFeePremium))
            colnames(plotdata) <- c("Period", "Scheduled Prin", "Prepaid Prin", "PT Interest", "Servicing", "PMI", "GFee")
            plotdata = melt(plotdata, id = "Period")
            
            plot <- ggplot(plotdata, aes(x= Period, y = value, fill = variable)) +
              geom_area() +
              theme_minimal()+
              scale_fill_brewer(palette = "Greys") +
              labs(fill = "") +
              ylab("Pool Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))+
              theme(legend.background = element_rect(fill = "white"))
            
            print(plot)
          }
)

setMethod("show", 
          signature(object = "BondAnalytics"),
          function(object)
          {
           cat("Bond Description", "\n")
           cat("BondId:"); print(object@ID)
           cat("Cusip:"); print(object@Cusip)
           cat("Coupon:"); print(object@Coupon)
           cat("Frequency:"); print(object@Frequency)
           cat("Basis:"); print(object@BondBasis)
           cat("Issue Date:"); print(object@IssueDate)
           cat("Last Payment Date:"); print(object@LastPmtDate)
           cat("Next Payment Date:"); print(object@NextPmtDate)
           cat("Maturity Date:"); print(object@Maturity)
           cat("Bond Valuation:", "\n")
           cat("Price:"); print(object@Price)
           cat("Accrued:"); print(object@Accrued)
           cat("Yield to Maturity:"); print(object@YieldToMaturity)
           cat("Risk Metrics:", "\n")
           cat("Weighted Average Life:"); print(object@WAL)
           cat("Modified Duration:"); print(unname(object@ModDuration))
           cat("Convexity:"); print(unname(object@Convexity))
           cat("Effective Duration"); print(unname(object@EffDuration))
           cat("Effective Convexity"); print(unname(object@EffConvexity))
           cat("Sector Detail:", "\n")
           cat("Bond Type:"); print(object@BondType)
           cat("Sector:"); print(object@Sector)
           cat("Moodys:"); print(object@Moody)
           cat("S&P:"); print(object@SP)
           cat("BondLab Rating:");print(object@BondLab)
           
           plotdata1 = as.data.frame(cbind(object@Period, object@TotalCashFlow))
           colnames(plotdata1) <- c("Period", "CashFlow")
           
           plot1 <- ggplot(plotdata1, aes(x= Period, y = CashFlow)) +
             geom_bar(stat = "identity", fill = "Grey") +
             theme_minimal() + 
             labs(fill = "") +
             ylab("Bond Cash Flow") +
             xlab("Period") +
             theme(axis.title.y=element_text(angle = 90, size = 20)) +
             theme(axis.text.y = element_text(angle = 90, size = 15)) +
             theme(axis.title.x=element_text(angle = 0, size = 20)) +
             theme(axis.text.x = element_text(angle = 0, size = 15)) +
             theme(legend.position = c(.82,.73))
           
           plotdata2 <- as.data.frame(cbind(object@KeyRateTenor, object@KeyRateDuration))
           colnames(plotdata2) <- c("KRTenor", "KRDuration")
           
           plot2 <- ggplot(plotdata2, aes(x = as.factor(KRTenor), y = KRDuration)) +
             geom_bar(stat = "identity", fill = "Grey") +
             theme_minimal() +
             labs(fill = "") +
             ylab("Key Rate Duration") +
             xlab("Key Rate Tenor") +
             theme(axis.title.y=element_text(angle = 90, size = 20)) +
             theme(axis.text.y = element_text(angle = 90, size = 15)) +
             theme(axis.title.x=element_text(angle = 0, size = 20)) +
             theme(axis.text.x = element_text(angle = 0, size = 15)) +
             theme(legend.position = c(.82,.73))
           
         multiplot(plot1, plot2, cols = 1)
           
          }
          )

setMethod("show", 
          signature(object = "PassThroughAnalytics"),
          function(object)
          {
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Effective Duration"); print(unname(object@EffDuration))
            cat("Effective Convexity"); print(unname(object@EffConvexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            plotdata1 = as.data.frame(cbind(object@Period, object@TotalCashFlow))
            colnames(plotdata1) <- c("Period", "CashFlow")
            
            plot1 <- ggplot(plotdata1, aes(x= Period, y = CashFlow)) +
              geom_bar(stat = "identity", width = 1, fill = "Grey") +
              theme_minimal() + 
              labs(fill = "") +
              ylab("Mtg. Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
            
            plotdata2 = as.data.frame(cbind(object@KeyRateTenor, object@KeyRateDuration))
            colnames(plotdata2) <- c("KRTenor", "KRDuration")
            
            plot2 <- ggplot(plotdata2, aes(x = as.factor(KRTenor), y = KRDuration)) +
              geom_bar(stat = "identity", fill = "Grey") +
              theme_minimal() +
              labs(fill = "") +
              ylab("Key Rate Duration") +
              xlab("Key Rate Tenor") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
                  
             multiplot(plot1, plot2, cols = 1)
            
          }
)

setMethod("show",
          signature(object = "DollarRoll"),
          function(object){
          cat("Dollar Roll Analysis", "\n")
          cat("Settlement Date"); print(object@SettlementDate)
          cat("Settlement Price"); print(object@Price)
          cat("Drop 32nds"); print(object@Drop)
          cat("Forward Settlement Date"); print(object@FwdSettlementDate)
          cat("Forward Price"); print(object@FwdPrice)
          cat("Beginning Market Value"); print(object@PrincipalProceeds)
          cat("Accrued Interest"); print(object@Accrued)
          cat("Roll Proceeds"); print(object@TotalProceeds)
          cat("Reinvestment Proceeds"); print(object@ReinvestmentIncome)
          cat("Future Value"); print(object@FutureValueRoll)
          cat("Dollar Advantage"); print(object@Advantage)
          #cat("Basis Points (Annualized"); print(object@BasisPoints)
          cat("Breakeven Drop"); print(object@DropImpliedValue)
          cat("Hold or Roll"); print(object@HoldorRoll)
          cat("Scheduled Principal"); print(object@ScheduledPrin)
          cat("Prepaid Principal"); print(object@PrepaidPrin)
          cat("Pass Through Interest"); print(object@PassThroughInterest)
          cat("Remaining Principal"); print(object@RemainingBalance)
          cat("Proceeds"); print(object@FuturePrincipalProceeds)
          cat("Hold Accrued"); print(object@FwdAccrued)
          cat("Future Value"); print(object@FutureValuePrinCarry)
          }
          )
  


