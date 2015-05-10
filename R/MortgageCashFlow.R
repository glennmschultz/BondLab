# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014 Bond Lab Technologies, Inc
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics"

setMethod("initialize",
          signature("MortgageCashFlow"),
          function(.Object,       
                   Price = numeric(),
                   Accrued = numeric(),
                   YieldToMaturity = numeric(),
                   WAL = numeric(),
                   ModDuration = numeric(),
                   Convexity = numeric(),
                   Period = numeric(),
                   PmtDate = "character",
                   TimePeriod = numeric(),
                   BeginningBal = numeric(),
                   MonthlyPmt = numeric(),
                   MonthlyInterest = numeric(),
                   PassThroughInterest = numeric(),
                   ScheduledPrin = numeric(),
                   PrepaidPrin = numeric(),
                   DefaultedPrin = numeric(),
                   LossAmount = numeric(),
                   RecoveredAmount = numeric(),
                   EndingBal = numeric(),
                   ServicingIncome = numeric(),
                   PMIPremium = numeric(),
                   GFeePremium = numeric(),  
                   TotalCashFlow = numeric()
          ){
            
            .Object@Price = Price
            .Object@Accrued = Accrued
            .Object@YieldToMaturity = YieldToMaturity
            .Object@WAL = WAL
            .Object@ModDuration = ModDuration
            .Object@Convexity = Convexity
            .Object@Period = Period
            .Object@PmtDate = PmtDate
            .Object@TimePeriod = TimePeriod
            .Object@BeginningBal = BeginningBal
            .Object@MonthlyPmt = MonthlyPmt
            .Object@MonthlyInterest = MonthlyInterest
            .Object@PassThroughInterest = PassThroughInterest
            .Object@ScheduledPrin = ScheduledPrin
            .Object@PrepaidPrin = PrepaidPrin
            .Object@DefaultedPrin = DefaultedPrin
            .Object@LossAmount = LossAmount
            .Object@RecoveredAmount = RecoveredAmount
            .Object@EndingBal = EndingBal
            .Object@ServicingIncome = ServicingIncome
            .Object@PMIPremium = PMIPremium
            .Object@GFeePremium = GFeePremium  
            .Object@TotalCashFlow = TotalCashFlow
            
            return(.Object)
            callNextMethod(.Object,...)            
          })


#'  A function to compute the cash flow of a pool of securitized mortgages
#' 
#' This is a generic function used to construct the class object MortgageCashFlow
#' For this function to work properly the classes MBSDetails and PrepaymentAssumption
#' must be present and loaded into the local environment
#' @param bond.id A character string referencing the object MBSDetails
#' @param original.bal The original balance of the MBS pool
#' @param settlement.date The settlment date of the MBS trade
#' @param price The price traded.  Price is input as a whole number.
#' For example $102 is input as 102.00 not 1.02.
#' @param PrepaymentAssumption A character string referencing the class object
#'  PrepaymentAssumption
#'  @examples
#'  \dontrun{
#'   MortgageCashFlow(bond.id = "bondlabMBS4", original.bal = 1000000000,
#'   settlement.date = "01/13/2013", price = 104.00, PrepaymentAssumption = "Prepayment")}
#' @export MortgageCashFlow
MortgageCashFlow <- function(bond.id = "character", 
                             original.bal = numeric(), 
                             settlement.date = "character", 
                             price = numeric(), 
                             PrepaymentAssumption = "character"){
  
  #This function error traps mortgage bond inputs
  ErrorTrap(bond.id = bond.id, 
            principal = original.balance, 
            settlement.date = settlement.date, 
            price = price)
  
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
  ncashflows = BondBasisConversion(issue.date = issue.date, 
                                   start.date = start.date, 
                                   end.date = end.date, 
                                   settlement.date = settlement.date,
                                   lastpmt.date = lastpmt.date, 
                                   nextpmt.date = end.date) 
  
  #Step2 build a vector of dates for the payment schedule
  # first get the pmtdate interval
  pmtdate.interval = months.in.year/frequency
  # then compute the payment dates
  pmtdate = as.Date(c(if(settlement.date == issue.date) 
  {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} else 
    {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y")
  
  #step3 build the time period vector (n) for discounting the cashflows 
  #nextpmt date is vector of payment dates to n for each period
  time.period = BondBasisConversion(issue.date = issue.date, 
                                    start.date = start.date, 
                                    end.date = end.date, 
                                    settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, 
                                    nextpmt.date = pmtdate)
  
  #step4 Count the number of cashflows 
  #num.periods is the total number of cashflows to be received
  #num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  
  #step5 initialize the prepayment model assumption class
  
  col.names <- c("Period", 
                 "Date", 
                 "Time", 
                 "Begin Bal", 
                 "Monthly Pmt", 
                 "Scheduled Int", 
                 "Scheduled Prin", 
                 "Prepaid Prin",
                 "Defaulted Prin",
                 "Loss Amount",
                 "Recovered Amount",
                 "Ending Bal", 
                 "Sevicing", 
                 "PMI", 
                 "GFee", 
                 "Pass Through Interest", 
                 "Investor CashFlow", 
                 "Present Value Factor", 
                 "Present Value", 
                 "Duration", 
                 "Convexity Time", 
                 "CashFlow Convexity", 
                 "Convexity")
  
  MBS.CF.Table <- array(data = NA, 
                        c(num.periods, 23), 
                        dimnames = list(seq(c(1:num.periods)),col.names))  
  
  for(x in 1:num.periods){
    MBS.CF.Table[x,1] = x
    MBS.CF.Table[x,2] = pmtdate[x] + delay
    MBS.CF.Table[x,3] = time.period[x]
    if (MBS.CF.Table[x,1] == 1) {MBS.CF.Table[x,4] = principal} else {MBS.CF.Table[x,4] = MBS.CF.Table[x-1,12]}
    MBS.CF.Table[x,5] = Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x,4], note.rate = note.rate, 
                                                 term.mos = (num.periods - MBS.CF.Table[x,1] + 1))
    
    MBS.CF.Table[x,6] = MBS.CF.Table[x,4] * (note.rate/12)
    MBS.CF.Table[x,7] = Sched.Prin(balance = MBS.CF.Table[x,4], note.rate = note.rate, 
                                   term.mos = (num.periods - MBS.CF.Table[x,1] + 1), period = 1)
    
    if(x != num.periods) {MBS.CF.Table[x,8] = PrepaymentAssumption@SMM[x] * (MBS.CF.Table[x,4] - MBS.CF.Table[x,7])} else                     
    {MBS.CF.Table[x,8] = 0}
  
    if(x!= num.periods) {MBS.CF.Table[x,9] = PrepaymentAssumption@MDR[x] * MBS.CF.Table[x,4]} else {MBS.CF.Table[x,9] = 0}
    MBS.CF.Table[x,10] = MBS.CF.Table[x,9] * 0 # Will need to include severity model here
    MBS.CF.Table[x,11] = MBS.CF.Table[x,9] - MBS.CF.Table[x,10]

    MBS.CF.Table[x,12] = MBS.CF.Table[x,4] - (MBS.CF.Table[x,7] + MBS.CF.Table[x,8] + MBS.CF.Table[x,9])
    MBS.CF.Table[x,13] = MBS.CF.Table[x,4] * (servicing.fee/1200)
    MBS.CF.Table[x,14] = MBS.CF.Table[x,4] * (pmi/1200)
    MBS.CF.Table[x,15] = MBS.CF.Table[x,4] * (g.fee/1200)
    MBS.CF.Table[x,16] = MBS.CF.Table[x,4] * (coupon/12)
    MBS.CF.Table[x,17] = MBS.CF.Table[x,7] + MBS.CF.Table[x,8] + MBS.CF.Table[x,11] + MBS.CF.Table[x,16]
  }
  
  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
                                         start.date = start.date, 
                                         end.date = end.date, 
                                         settlement.date = settlement.date,
                                         lastpmt.date = lastpmt.date,
                                         nextpmt.date = nextpmt.date)) * 360
  
  days.between.pmtdate = ((12/frequency)/12) * 360
  days.of.accrued = (days.between.pmtdate - days.to.nextpmt) 
  accrued.interest = (days.of.accrued/days.between.pmtdate) * MBS.CF.Table[1,16]
  #note the MBS.CF.Table[1,16] should be replaced by orig.bal * factor * coupon (?)
  
  #Step6 solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  #it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value 
  irr <- function(rate , time.period , cashflow , principal , price , accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  
  ytm = uniroot(irr, interval = c(lower = -.75, upper = .75), tol =.0000000001, 
                time.period = MBS.CF.Table[,3], 
                cashflow = MBS.CF.Table[,17], 
                principal = principal, 
                price = price, 
                accrued.interest = accrued.interest)$root

  
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency
  
  #Step7 Present value of the cash flows Present Value Factors
  MBS.CF.Table[,18] = 1/((1+(Yield.To.Maturity/frequency))^(MBS.CF.Table[,3] * frequency))
  
  #Present Value of the cash flows
  MBS.CF.Table[,19] = MBS.CF.Table[,17] * MBS.CF.Table[,18]
  
  #Step8 Risk measures Duration Factors
  MBS.CF.Table[,20] = MBS.CF.Table[,3] * (MBS.CF.Table[,19]/((principal * price) + accrued.interest))
  
  #Convexity Factors
  MBS.CF.Table[,21] = MBS.CF.Table[,3] *(MBS.CF.Table[,3] + 1)
  
  MBS.CF.Table[,22] = (MBS.CF.Table[,17]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((MBS.CF.Table[,3] + 2) * frequency)))/ 
    ((principal * price) + accrued.interest)
  
  MBS.CF.Table[,23] = MBS.CF.Table[,21] * MBS.CF.Table[,22] 
  
  #Weighted Average Life
  WAL = sum((((MBS.CF.Table[,7]) + (MBS.CF.Table[,8])) * MBS.CF.Table[,3])/ sum((MBS.CF.Table[,7]) + (MBS.CF.Table[,8])))
  
  #Duration and Convexity
  Duration = apply(MBS.CF.Table, 2, sum)[20]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(MBS.CF.Table, 2, sum)[23] * .5
  
  #Create Class Mortgage Loan Cashflows
  new("MortgageCashFlow",
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
      PassThroughInterest = MBS.CF.Table[,16],
      ScheduledPrin = MBS.CF.Table[,7],
      PrepaidPrin = MBS.CF.Table[,8],
      DefaultedPrin = MBS.CF.Table[,9],
      LossAmount = MBS.CF.Table[,10],
      RecoveredAmount = MBS.CF.Table[,11],
      EndingBal = MBS.CF.Table[,12],
      ServicingIncome = MBS.CF.Table[,13],
      PMIPremium = MBS.CF.Table[,14],    
      GFeePremium = MBS.CF.Table[,15],
      TotalCashFlow = MBS.CF.Table[,17]
  )
}

setGeneric("MortgageCashFlow", function(bond.id = "character", 
                                        original.bal = numeric(), 
                                        settlement.date = "character", 
                                        price = numeric(), 
                                        PrepaymentAssumption = "character") 
{standardGeneric("MortgageCashFlow")})







