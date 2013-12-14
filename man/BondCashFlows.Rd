\name{BondCashFlows}
\alias{BondCashFlows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BondCashFlows
}
\description{
A constructor function for the class BondCashFlows
}
\usage{
BondCashFlows(bond.id = "character", principal = numeric(), settlement.date = "character", price = numeric())
}

\arguments{
  \item{bond.id}{
The bond cusip or psudeo cusip number
}
  \item{principal}{
Principal amount of the bond purchased by the investor
}
  \item{settlement.date}{
settlement date of the bond 
}
  \item{price}{
price is the bond price paid by the investor
}
}
\details{
The function returns the class bond cash flows
}
\value{
returns the class bond cash flows
}
\references{
Investing in Mortgage and Asset Backed Securities using R and Bond Lan (2015), Glenn Schultz
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
BondCashFlows(bond.id = "bondlab10", principal = 100000, settlement.date = "01-15-2013", price = 100)

## The function is currently defined as
structure(function (bond.id = "character", principal = numeric(), 
    settlement.date = "character", price = numeric()) 
{
    standardGeneric("BondCashFlows")
  }, generic = structure("BondCashFlows", package = "BondLab"), package = "BondLab", group = list(), valueClass = character(0), signature = c("bond.id", 
"principal", "settlement.date", "price"), default = structure(function (bond.id = "character", 
    principal = numeric(), settlement.date = "character", price = numeric()) 
{
    bond.id <- readRDS(paste("~/BondLab/BondData/", bond.id, 
        ".rds", sep = ""))
    issue.date = as.Date(bond.id@IssueDate, "\%m-\%d-\%Y")
    start.date = as.Date(bond.id@DatedDate, "\%m-\%d-\%Y")
    end.date = as.Date(bond.id@Maturity, "\%m-\%d-\%Y")
    lastpmt.date = as.Date(bond.id@LastPmtDate, "\%m-\%d-\%Y")
    nextpmt.date = as.Date(bond.id@NextPmtDate, "\%m-\%d-\%Y")
    coupon = bond.id@Coupon
    frequency = bond.id@Frequency
    settlement.date = as.Date(c(settlement.date), "\%m-\%d-\%Y")
    ErrorTrap(bond.id = bond.id, principal = principal, settlement.date = settlement.date, 
        price = price)
    if (price <= 1) {
        price = price
    }
    else {
        price = price/100
    }
    if (coupon > 0) {
        coupon = coupon/100
    }
    else {
        coupon = coupon
    }
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
    col.names <- c("Period", "Date", "Time", "Principal Outstanding", 
        "Coupon", "Coupon Income", "Principal Paid", "TotalCashFlow", 
        "Present Value Factor", "Present Value", "Duration", 
        "Convexity Time", "CashFlow Convexity", "Convexity")
    Bond.CF.Table <- array(data = NA, c(num.periods, 14), dimnames = list(seq(c(1:num.periods)), 
        col.names))
    for (i in 1:num.periods) {
        Bond.CF.Table[i, 1] = i
        Bond.CF.Table[i, 2] = pmtdate[i]
        Bond.CF.Table[i, 3] = time.period[i]
        Bond.CF.Table[i, 4] = principal
        Bond.CF.Table[i, 5] = coupon/frequency
        Bond.CF.Table[i, 6] = Bond.CF.Table[i, 5] * Bond.CF.Table[i, 
            4]
        if (Bond.CF.Table[i, 2] == end.date) {
            Bond.CF.Table[i, 7] = principal
        }
        else {
            Bond.CF.Table[i, 7] = 0
        }
        Bond.CF.Table[i, 8] = Bond.CF.Table[i, 6] + Bond.CF.Table[i, 
            7]
    }
    days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)) * 360
    days.between.pmtdate = ((12/frequency)/12) * 360
    days.of.acrrued = days.between.pmtdate - days.to.nextpmt
    acrrued.interest = (days.of.acrrued/days.between.pmtdate) * 
        Bond.CF.Table[1, 6]
    irr <- function(rate, time.period, cashflow, principal, price, 
        acrrued.interest) {
        pv = cashflow * 1/(1 + rate)^time.period
        proceeds = principal * price
        sum(pv) - (proceeds + acrrued.interest)
    }
    ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol = 1e-09, 
        time.period = Bond.CF.Table[, 3], cashflow = Bond.CF.Table[, 
            8], principal = principal, price = price, acrrued.interest = acrrued.interest)$root
    Yield.To.Maturity = (((1 + ytm)^(1/frequency)) - 1) * frequency
    Bond.CF.Table[, 9] = 1/((1 + (Yield.To.Maturity/frequency))^(Bond.CF.Table[, 
        3] * frequency))
    Bond.CF.Table[, 10] = Bond.CF.Table[, 8] * Bond.CF.Table[, 
        9]
    Bond.CF.Table[, 11] = Bond.CF.Table[, 3] * (Bond.CF.Table[, 
        10]/((principal * price) + acrrued.interest))
    Bond.CF.Table[, 12] = Bond.CF.Table[, 3] * (Bond.CF.Table[, 
        3] + 1)
    Bond.CF.Table[, 13] = (Bond.CF.Table[, 8]/((1 + ((Yield.To.Maturity)/frequency))^((Bond.CF.Table[, 
        3] + 2) * frequency)))/((principal * price) + acrrued.interest)
    Bond.CF.Table[, 14] = Bond.CF.Table[, 12] * Bond.CF.Table[, 
        13]
    WAL = sum((Bond.CF.Table[, 7] * Bond.CF.Table[, 3]))/sum(Bond.CF.Table[, 
        7])
    Duration = apply(Bond.CF.Table, 2, sum)[11]
    Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
    Convexity = apply(Bond.CF.Table, 2, sum)[14] * 0.5
    new("BondCashFlows", Price = price * 100, Acrrued = acrrued.interest, 
        YieldToMaturity = Yield.To.Maturity, WAL = WAL, ModDuration = Modified.Duration, 
        Convexity = Convexity, Period = Bond.CF.Table[, 1], PmtDate = as.character(as.Date(Bond.CF.Table[, 
            2], origin = "1970-01-01")), TimePeriod = Bond.CF.Table[, 
            3], PrincipalOutstanding = Bond.CF.Table[, 4], CouponPmt = Bond.CF.Table[, 
            5], TotalCashFlow = Bond.CF.Table[, 8], bond.id)
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("BondCashFlows", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (bond.id = "character", 
    principal = numeric(), settlement.date = "character", price = numeric()) 
{
    bond.id <- readRDS(paste("~/BondLab/BondData/", bond.id, 
        ".rds", sep = ""))
    issue.date = as.Date(bond.id@IssueDate, "\%m-\%d-\%Y")
    start.date = as.Date(bond.id@DatedDate, "\%m-\%d-\%Y")
    end.date = as.Date(bond.id@Maturity, "\%m-\%d-\%Y")
    lastpmt.date = as.Date(bond.id@LastPmtDate, "\%m-\%d-\%Y")
    nextpmt.date = as.Date(bond.id@NextPmtDate, "\%m-\%d-\%Y")
    coupon = bond.id@Coupon
    frequency = bond.id@Frequency
    settlement.date = as.Date(c(settlement.date), "\%m-\%d-\%Y")
    ErrorTrap(bond.id = bond.id, principal = principal, settlement.date = settlement.date, 
        price = price)
    if (price <= 1) {
        price = price
    }
    else {
        price = price/100
    }
    if (coupon > 0) {
        coupon = coupon/100
    }
    else {
        coupon = coupon
    }
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
    col.names <- c("Period", "Date", "Time", "Principal Outstanding", 
        "Coupon", "Coupon Income", "Principal Paid", "TotalCashFlow", 
        "Present Value Factor", "Present Value", "Duration", 
        "Convexity Time", "CashFlow Convexity", "Convexity")
    Bond.CF.Table <- array(data = NA, c(num.periods, 14), dimnames = list(seq(c(1:num.periods)), 
        col.names))
    for (i in 1:num.periods) {
        Bond.CF.Table[i, 1] = i
        Bond.CF.Table[i, 2] = pmtdate[i]
        Bond.CF.Table[i, 3] = time.period[i]
        Bond.CF.Table[i, 4] = principal
        Bond.CF.Table[i, 5] = coupon/frequency
        Bond.CF.Table[i, 6] = Bond.CF.Table[i, 5] * Bond.CF.Table[i, 
            4]
        if (Bond.CF.Table[i, 2] == end.date) {
            Bond.CF.Table[i, 7] = principal
        }
        else {
            Bond.CF.Table[i, 7] = 0
        }
        Bond.CF.Table[i, 8] = Bond.CF.Table[i, 6] + Bond.CF.Table[i, 
            7]
    }
    days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
        start.date = start.date, end.date = end.date, settlement.date = settlement.date, 
        lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date, 
        coupon = coupon, principal = principal, frequency = frequency, 
        price = price)) * 360
    days.between.pmtdate = ((12/frequency)/12) * 360
    days.of.acrrued = days.between.pmtdate - days.to.nextpmt
    acrrued.interest = (days.of.acrrued/days.between.pmtdate) * 
        Bond.CF.Table[1, 6]
    irr <- function(rate, time.period, cashflow, principal, price, 
        acrrued.interest) {
        pv = cashflow * 1/(1 + rate)^time.period
        proceeds = principal * price
        sum(pv) - (proceeds + acrrued.interest)
    }
    ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol = 1e-09, 
        time.period = Bond.CF.Table[, 3], cashflow = Bond.CF.Table[, 
            8], principal = principal, price = price, acrrued.interest = acrrued.interest)$root
    Yield.To.Maturity = (((1 + ytm)^(1/frequency)) - 1) * frequency
    Bond.CF.Table[, 9] = 1/((1 + (Yield.To.Maturity/frequency))^(Bond.CF.Table[, 
        3] * frequency))
    Bond.CF.Table[, 10] = Bond.CF.Table[, 8] * Bond.CF.Table[, 
        9]
    Bond.CF.Table[, 11] = Bond.CF.Table[, 3] * (Bond.CF.Table[, 
        10]/((principal * price) + acrrued.interest))
    Bond.CF.Table[, 12] = Bond.CF.Table[, 3] * (Bond.CF.Table[, 
        3] + 1)
    Bond.CF.Table[, 13] = (Bond.CF.Table[, 8]/((1 + ((Yield.To.Maturity)/frequency))^((Bond.CF.Table[, 
        3] + 2) * frequency)))/((principal * price) + acrrued.interest)
    Bond.CF.Table[, 14] = Bond.CF.Table[, 12] * Bond.CF.Table[, 
        13]
    WAL = sum((Bond.CF.Table[, 7] * Bond.CF.Table[, 3]))/sum(Bond.CF.Table[, 
        7])
    Duration = apply(Bond.CF.Table, 2, sum)[11]
    Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
    Convexity = apply(Bond.CF.Table, 2, sum)[14] * 0.5
    new("BondCashFlows", Price = price * 100, Acrrued = acrrued.interest, 
        YieldToMaturity = Yield.To.Maturity, WAL = WAL, ModDuration = Modified.Duration, 
        Convexity = Convexity, Period = Bond.CF.Table[, 1], PmtDate = as.character(as.Date(Bond.CF.Table[, 
            2], origin = "1970-01-01")), TimePeriod = Bond.CF.Table[, 
            3], PrincipalOutstanding = Bond.CF.Table[, 4], CouponPmt = Bond.CF.Table[, 
            5], TotalCashFlow = Bond.CF.Table[, 8], bond.id)
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("BondCashFlows", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")))(bond.id, 
    principal, settlement.date, price), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bond }
\keyword{ Cash Flow }% __ONLY ONE__ keyword per line
