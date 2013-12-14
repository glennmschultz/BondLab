\name{MortgageCashFlows}
\alias{MortgageCashFlows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MortgageCashFlows
}
\description{
A constructor function for the class MortgageCashFlows
}
\usage{
MortgageCashFlows(bond.id = "character", original.bal = numeric(), settlement.date = "character", price = numeric(), PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bond.id}{
The bond cusip or psudeo cusip number
}
  \item{original.bal}{
The original balance of the mortgage security. The trading factor is stored in the bond cusip file and passed to MortgageCashFlows
}
  \item{settlement.date}{
settlement date of the MBS
}
  \item{price}{
price is the bond price paid by the investor
}
  \item{PrepaymentAssumption}{
The prepayment assumption used to derive the MBS cash flow.  Valid inputs are PPC assumption, model, ,or CPR 
}
  \item{\dots}{
allows for evaluation of the below
}
  \item{begin.cpr}{
The starting CPR used when PrepaymentAssumption = "PPC"
}
  \item{end.cpr}{
The peak CPR reached at the end of the seasoning period
}
  \item{seasoning.period}{
The lenght of the seasoning period
}
  \item{CPR}{
Constant Prepayment Rate used when PrepaymentAssumption = "CPR"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securities using R and BondLab (2015), Glenn Schultz
}
\author{
Glenn Schultz, CFA
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
MortgageCashFlows(bond.id = "bondlabMBS4", original.bal = 100000, settlement.date = "01-13-2013", price = 100, PrepaymentAssumption = "PPC", begin.cpr = .2, end.cpr = 6, seasoning.period = 30) 

## The function is currently defined as
structure(function (bond.id = "character", original.bal = numeric(), 
    settlement.date = "character", price = numeric(), PrepaymentAssumption = "character", 
    ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), 
    CPR = numeric()) 
{
    standardGeneric("MortgageCashFlows")
  }, generic = structure("MortgageCashFlows", package = "BondLab"), package = "BondLab", group = list(), valueClass = character(0), signature = c("bond.id", 
"original.bal", "settlement.date", "price", "PrepaymentAssumption", 
"begin.cpr", "end.cpr", "seasoning.period", "CPR"), default = structure(function (bond.id = "character", 
    original.bal = numeric(), settlement.date = "character", 
    price = numeric(), PrepaymentAssumption = "character", ..., 
    begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), 
    CPR = numeric()) 
{
    if (!PrepaymentAssumption \%in\% c("MODEL", "CPR", "PPC")) 
        stop("Not a Valid Prepayment Assumption")
    bond.id <- readRDS(paste("~/BondLab/BondData/", bond.id, 
        ".rds", sep = ""))
    ErrorTrap(bond.id = bond.id, principal = original.balance, 
        settlement.date = settlement.date, price = price)
    issue.date = as.Date(bond.id@IssueDate, "\%m-\%d-\%Y")
    start.date = as.Date(bond.id@DatedDate, "\%m-\%d-\%Y")
    end.date = as.Date(bond.id@Maturity, "\%m-\%d-\%Y")
    lastpmt.date = as.Date(bond.id@LastPmtDate, "\%m-\%d-\%Y")
    nextpmt.date = as.Date(bond.id@NextPmtDate, "\%m-\%d-\%Y")
    coupon = bond.id@Coupon
    frequency = bond.id@Frequency
    delay = bond.id@PaymentDelay
    settlement.date = as.Date(c(settlement.date), "\%m-\%d-\%Y")
    note.rate = bond.id@GWac
    servicing.fee = bond.id@Servicing
    pmi = bond.id@PMI
    g.fee = bond.id@Gfee
    if (price <= 1) {
        price = price
    }
    else {
        price = price/100
    }
    if (coupon > 1) {
        coupon = coupon/100
    }
    else {
        coupon = coupon
    }
    if (note.rate > 1) {
        note.rate = note.rate/100
    }
    else {
        note.rate = note.rate
    }
    factor = bond.id@MBSFactor
    principal = original.bal * factor
    ncashflows = BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = end.date, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)
    pmtdate.interval = 12/frequency
    pmtdate = as.Date(c(if (settlement.date == issue.date) {
        seq(start.date, end.date, by = paste(pmtdate.interval, 
            "months"))
    } else {
        seq(nextpmt.date, end.date, by = paste(pmtdate.interval, 
            "months"))
    }), "\%m-\%d-\%Y")
    time.period = BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = pmtdate, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)
    num.periods = length(time.period)
    col.names <- c("Period", "Date", "Time", "Begin Bal", "Monthly Pmt", 
        "Scheduled Int", "Scheduled Prin", "Prepaid Prin", "Ending Bal", 
        "Sevicing", "PMI", "GFee", "Pass Through Interest", "Investor CashFlow", 
        "Present Value Factor", "Present Value", "Duration", 
        "Convexity Time", "CashFlow Convexity", "Convexity")
    MBS.CF.Table <- array(data = NA, c(num.periods, 20), dimnames = list(seq(c(1:num.periods)), 
        col.names))
    for (x in 1:num.periods) {
        MBS.CF.Table[x, 1] = x
        MBS.CF.Table[x, 2] = pmtdate[x] + delay
        MBS.CF.Table[x, 3] = time.period[x]
        if (MBS.CF.Table[x, 1] == 1) {
            MBS.CF.Table[x, 4] = principal
        }
        else {
            MBS.CF.Table[x, 4] = MBS.CF.Table[x - 1, 9]
        }
        MBS.CF.Table[x, 5] = Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x, 
            4], note.rate = note.rate, term.mos = (num.periods - 
            MBS.CF.Table[x, 1] + 1))
        MBS.CF.Table[x, 6] = MBS.CF.Table[x, 4] * (note.rate/12)
        MBS.CF.Table[x, 7] = Sched.Prin(balance = MBS.CF.Table[x, 
            4], note.rate = note.rate, term.mos = (num.periods - 
            MBS.CF.Table[x, 1] + 1), period = 1, payment = MBS.CF.Table[x, 
            5])
        if (x != num.periods) {
            MBS.CF.Table[x, 8] = if (PrepaymentAssumption == 
                "PPC") {
                (1 - (1 - PPC.Ramp(begin.cpr = begin.cpr, end.cpr = end.cpr, 
                  season.period = seasoning.period, period = x))^(1/12)) * 
                  (MBS.CF.Table[x, 4] - MBS.CF.Table[x, 7])
            }
            else {
                if (PrepaymentAssumption == "CPR") {
                  (1 - (1 - CPR)^(1/12)) * (MBS.CF.Table[x, 4] - 
                    MBS.CF.Table[x, 7])
                }
            }
        }
        else {
            MBS.CF.Table[x, 8] = 0
        }
        MBS.CF.Table[x, 9] = MBS.CF.Table[x, 4] - MBS.CF.Table[x, 
            7] - MBS.CF.Table[x, 8]
        MBS.CF.Table[x, 10] = MBS.CF.Table[x, 4] * (servicing.fee/1200)
        MBS.CF.Table[x, 11] = MBS.CF.Table[x, 4] * (pmi/1200)
        MBS.CF.Table[x, 12] = MBS.CF.Table[x, 4] * (g.fee/1200)
        MBS.CF.Table[x, 13] = MBS.CF.Table[x, 4] * (coupon/12)
        MBS.CF.Table[x, 14] = MBS.CF.Table[x, 13] + MBS.CF.Table[x, 
            7] + MBS.CF.Table[x, 8]
    }
    days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)) * 360
    days.between.pmtdate = ((12/frequency)/12) * 360
    days.of.acrrued = (days.between.pmtdate - days.to.nextpmt)
    acrrued.interest = (days.of.acrrued/days.between.pmtdate) * 
        MBS.CF.Table[1, 6]
    irr <- function(rate, time.period, cashflow, principal, price, 
        acrrued.interest) {
        pv = cashflow * 1/(1 + rate)^time.period
        proceeds = principal * price
        sum(pv) - (proceeds + acrrued.interest)
    }
    ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol = 0.000000001, 
        time.period = MBS.CF.Table[, 3], cashflow = MBS.CF.Table[, 
            14], principal = principal, price = price, acrrued.interest = acrrued.interest)$root
    Yield.To.Maturity = (((1 + ytm)^(1/frequency)) - 1) * frequency
    MBS.CF.Table[, 15] = 1/((1 + (Yield.To.Maturity/frequency))^(MBS.CF.Table[, 
        3] * frequency))
    MBS.CF.Table[, 16] = MBS.CF.Table[, 14] * MBS.CF.Table[, 
        15]
    MBS.CF.Table[, 17] = MBS.CF.Table[, 3] * (MBS.CF.Table[, 
        16]/((principal * price) + acrrued.interest))
    MBS.CF.Table[, 18] = MBS.CF.Table[, 3] * (MBS.CF.Table[, 
        3] + 1)
    MBS.CF.Table[, 19] = (MBS.CF.Table[, 14]/((1 + ((Yield.To.Maturity)/frequency))^((MBS.CF.Table[, 
        3] + 2) * frequency)))/((principal * price) + acrrued.interest)
    MBS.CF.Table[, 20] = MBS.CF.Table[, 18] * MBS.CF.Table[, 
        19]
    WAL = sum((((MBS.CF.Table[, 5]) + (MBS.CF.Table[, 7])) * 
        MBS.CF.Table[, 3])/sum((MBS.CF.Table[, 7]) + (MBS.CF.Table[, 
        5])))
    Duration = apply(MBS.CF.Table, 2, sum)[17]
    Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
    Convexity = apply(MBS.CF.Table, 2, sum)[20] * 0.5
    new("MortgageCashFlows", bond.id, Price = price * 100, Acrrued = acrrued.interest, 
        YieldToMaturity = Yield.To.Maturity, WAL = WAL, ModDuration = Modified.Duration, 
        Convexity = Convexity, Period = MBS.CF.Table[, 1], PmtDate = as.character(as.Date(MBS.CF.Table[, 
            2], origin = "1970-01-01")), TimePeriod = MBS.CF.Table[, 
            3], BeginningBal = MBS.CF.Table[, 4], MonthlyPmt = MBS.CF.Table[, 
            5], MonthlyInterest = MBS.CF.Table[, 6], PassThroughInterest = MBS.CF.Table[, 
            13], ScheduledPrin = MBS.CF.Table[, 7], SMM = 999, 
        PrepaidPrin = MBS.CF.Table[, 8], EndingBal = MBS.CF.Table[, 
            9], ServicingIncome = MBS.CF.Table[, 10], PMIPremium = MBS.CF.Table[, 
            11], GFeePremium = MBS.CF.Table[, 12], TotalCashFlow = MBS.CF.Table[, 
            14])
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("MortgageCashFlows", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (bond.id = "character", 
    original.bal = numeric(), settlement.date = "character", 
    price = numeric(), PrepaymentAssumption = "character", ..., 
    begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), 
    CPR = numeric()) 
{
    if (!PrepaymentAssumption \%in\% c("MODEL", "CPR", "PPC")) 
        stop("Not a Valid Prepayment Assumption")
    bond.id <- readRDS(paste("~/BondLab/BondData/", bond.id, 
        ".rds", sep = ""))
    ErrorTrap(bond.id = bond.id, principal = original.balance, 
        settlement.date = settlement.date, price = price)
    issue.date = as.Date(bond.id@IssueDate, "\%m-\%d-\%Y")
    start.date = as.Date(bond.id@DatedDate, "\%m-\%d-\%Y")
    end.date = as.Date(bond.id@Maturity, "\%m-\%d-\%Y")
    lastpmt.date = as.Date(bond.id@LastPmtDate, "\%m-\%d-\%Y")
    nextpmt.date = as.Date(bond.id@NextPmtDate, "\%m-\%d-\%Y")
    coupon = bond.id@Coupon
    frequency = bond.id@Frequency
    delay = bond.id@PaymentDelay
    settlement.date = as.Date(c(settlement.date), "\%m-\%d-\%Y")
    note.rate = bond.id@GWac
    servicing.fee = bond.id@Servicing
    pmi = bond.id@PMI
    g.fee = bond.id@Gfee
    if (price <= 1) {
        price = price
    }
    else {
        price = price/100
    }
    if (coupon > 1) {
        coupon = coupon/100
    }
    else {
        coupon = coupon
    }
    if (note.rate > 1) {
        note.rate = note.rate/100
    }
    else {
        note.rate = note.rate
    }
    factor = bond.id@MBSFactor
    principal = original.bal * factor
    ncashflows = BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = end.date, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)
    pmtdate.interval = 12/frequency
    pmtdate = as.Date(c(if (settlement.date == issue.date) {
        seq(start.date, end.date, by = paste(pmtdate.interval, 
            "months"))
    } else {
        seq(nextpmt.date, end.date, by = paste(pmtdate.interval, 
            "months"))
    }), "\%m-\%d-\%Y")
    time.period = BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = pmtdate, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)
    num.periods = length(time.period)
    col.names <- c("Period", "Date", "Time", "Begin Bal", "Monthly Pmt", 
        "Scheduled Int", "Scheduled Prin", "Prepaid Prin", "Ending Bal", 
        "Sevicing", "PMI", "GFee", "Pass Through Interest", "Investor CashFlow", 
        "Present Value Factor", "Present Value", "Duration", 
        "Convexity Time", "CashFlow Convexity", "Convexity")
    MBS.CF.Table <- array(data = NA, c(num.periods, 20), dimnames = list(seq(c(1:num.periods)), 
        col.names))
    for (x in 1:num.periods) {
        MBS.CF.Table[x, 1] = x
        MBS.CF.Table[x, 2] = pmtdate[x] + delay
        MBS.CF.Table[x, 3] = time.period[x]
        if (MBS.CF.Table[x, 1] == 1) {
            MBS.CF.Table[x, 4] = principal
        }
        else {
            MBS.CF.Table[x, 4] = MBS.CF.Table[x - 1, 9]
        }
        MBS.CF.Table[x, 5] = Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x, 
            4], note.rate = note.rate, term.mos = (num.periods - 
            MBS.CF.Table[x, 1] + 1))
        MBS.CF.Table[x, 6] = MBS.CF.Table[x, 4] * (note.rate/12)
        MBS.CF.Table[x, 7] = Sched.Prin(balance = MBS.CF.Table[x, 
            4], note.rate = note.rate, term.mos = (num.periods - 
            MBS.CF.Table[x, 1] + 1), period = 1, payment = MBS.CF.Table[x, 
            5])
        if (x != num.periods) {
            MBS.CF.Table[x, 8] = if (PrepaymentAssumption == 
                "PPC") {
                (1 - (1 - PPC.Ramp(begin.cpr = begin.cpr, end.cpr = end.cpr, 
                  season.period = seasoning.period, period = x))^(1/12)) * 
                  (MBS.CF.Table[x, 4] - MBS.CF.Table[x, 7])
            }
            else {
                if (PrepaymentAssumption == "CPR") {
                  (1 - (1 - CPR)^(1/12)) * (MBS.CF.Table[x, 4] - 
                    MBS.CF.Table[x, 7])
                }
            }
        }
        else {
            MBS.CF.Table[x, 8] = 0
        }
        MBS.CF.Table[x, 9] = MBS.CF.Table[x, 4] - MBS.CF.Table[x, 
            7] - MBS.CF.Table[x, 8]
        MBS.CF.Table[x, 10] = MBS.CF.Table[x, 4] * (servicing.fee/1200)
        MBS.CF.Table[x, 11] = MBS.CF.Table[x, 4] * (pmi/1200)
        MBS.CF.Table[x, 12] = MBS.CF.Table[x, 4] * (g.fee/1200)
        MBS.CF.Table[x, 13] = MBS.CF.Table[x, 4] * (coupon/12)
        MBS.CF.Table[x, 14] = MBS.CF.Table[x, 13] + MBS.CF.Table[x, 
            7] + MBS.CF.Table[x, 8]
    }
    days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)) * 360
    days.between.pmtdate = ((12/frequency)/12) * 360
    days.of.acrrued = (days.between.pmtdate - days.to.nextpmt)
    acrrued.interest = (days.of.acrrued/days.between.pmtdate) * 
        MBS.CF.Table[1, 6]
    irr <- function(rate, time.period, cashflow, principal, price, 
        acrrued.interest) {
        pv = cashflow * 1/(1 + rate)^time.period
        proceeds = principal * price
        sum(pv) - (proceeds + acrrued.interest)
    }
    ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol = 0.000000001, 
        time.period = MBS.CF.Table[, 3], cashflow = MBS.CF.Table[, 
            14], principal = principal, price = price, acrrued.interest = acrrued.interest)$root
    Yield.To.Maturity = (((1 + ytm)^(1/frequency)) - 1) * frequency
    MBS.CF.Table[, 15] = 1/((1 + (Yield.To.Maturity/frequency))^(MBS.CF.Table[, 
        3] * frequency))
    MBS.CF.Table[, 16] = MBS.CF.Table[, 14] * MBS.CF.Table[, 
        15]
    MBS.CF.Table[, 17] = MBS.CF.Table[, 3] * (MBS.CF.Table[, 
        16]/((principal * price) + acrrued.interest))
    MBS.CF.Table[, 18] = MBS.CF.Table[, 3] * (MBS.CF.Table[, 
        3] + 1)
    MBS.CF.Table[, 19] = (MBS.CF.Table[, 14]/((1 + ((Yield.To.Maturity)/frequency))^((MBS.CF.Table[, 
        3] + 2) * frequency)))/((principal * price) + acrrued.interest)
    MBS.CF.Table[, 20] = MBS.CF.Table[, 18] * MBS.CF.Table[, 
        19]
    WAL = sum((((MBS.CF.Table[, 5]) + (MBS.CF.Table[, 7])) * 
        MBS.CF.Table[, 3])/sum((MBS.CF.Table[, 7]) + (MBS.CF.Table[, 
        5])))
    Duration = apply(MBS.CF.Table, 2, sum)[17]
    Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
    Convexity = apply(MBS.CF.Table, 2, sum)[20] * 0.5
    new("MortgageCashFlows", bond.id, Price = price * 100, Acrrued = acrrued.interest, 
        YieldToMaturity = Yield.To.Maturity, WAL = WAL, ModDuration = Modified.Duration, 
        Convexity = Convexity, Period = MBS.CF.Table[, 1], PmtDate = as.character(as.Date(MBS.CF.Table[, 
            2], origin = "1970-01-01")), TimePeriod = MBS.CF.Table[, 
            3], BeginningBal = MBS.CF.Table[, 4], MonthlyPmt = MBS.CF.Table[, 
            5], MonthlyInterest = MBS.CF.Table[, 6], PassThroughInterest = MBS.CF.Table[, 
            13], ScheduledPrin = MBS.CF.Table[, 7], SMM = 999, 
        PrepaidPrin = MBS.CF.Table[, 8], EndingBal = MBS.CF.Table[, 
            9], ServicingIncome = MBS.CF.Table[, 10], PMIPremium = MBS.CF.Table[, 
            11], GFeePremium = MBS.CF.Table[, 12], TotalCashFlow = MBS.CF.Table[, 
            14])
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("MortgageCashFlows", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")))(bond.id, 
    original.bal, settlement.date, price, PrepaymentAssumption, 
    ..., begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, 
    CPR = CPR), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
