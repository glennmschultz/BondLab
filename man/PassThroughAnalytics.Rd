\name{PassThroughAnalytics}
\alias{PassThroughAnalytics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PassThroughAnalytics
}
\description{
Analytics for MBS pass through pool
}
\usage{
PassThroughAnalytics(bond.id = "character", original.bal = numeric(), price = numeric(), trade.date = "character", settlement.date = "character", method = method, PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bond.id}{
The bond cusip or psudeo cusip number
}
  \item{original.bal}{
Original balance of the pool without respect to the current factor
}
  \item{price}{
price is the bond price paid by the investor
}
  \item{trade.date}{
The date on which the trade was intiated and the agreed price set
}
  \item{settlement.date}{
settlement date of the bond
}
  \item{method}{
The method used to fit the term strucutre.  The default is Nelson-Siegel.  Other methods include Severson, Dibold-Lee, and Muclloch Cubic Spline.
}
  \item{PrepaymentAssumption}{
Prepayment assumption used to drive the cash flow.  Methods include PPC, CPR, and Model
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{begin.cpr}{
For the PPC assumption, starting CPR 
}
  \item{end.cpr}{
For the PPC assumption, end CPR
}
  \item{seasoning.period}{
The length of the seasoning ramp
}
  \item{CPR}{
Constant Prepayment Rate
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
PassThroughAnalytics(bond.id = "bondlabMBS4", original.bal = 100000, price = 100, trade.date = "01-10-2013", settlement.date = "01-13-2013", PrepaymentAssumption = "PPC", begin.cpr = .2, end.cpr = 6, seasoning.period = 30)


## The function is currently defined as
structure(function (bond.id = "character", original.bal = numeric(), 
    price = numeric(), trade.date = "character", settlement.date = "character", 
    method = method, PrepaymentAssumption = "character", ..., 
    begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), 
    CPR = numeric()) 
{
    standardGeneric("PassThroughAnalytics")
  }, generic = structure("PassThroughAnalytics", package = "BondLab"), package = "BondLab", group = list(), valueClass = character(0), signature = c("bond.id", 
"original.bal", "price", "trade.date", "settlement.date", "method", 
"PrepaymentAssumption", "begin.cpr", "end.cpr", "seasoning.period", 
"CPR"), default = structure(function (bond.id = "character", 
    original.bal = numeric(), price = numeric(), trade.date = "character", 
    settlement.date = "character", method = method, PrepaymentAssumption = "character", 
    ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), 
    CPR = numeric()) 
{
    if (trade.date > settlement.date) 
        stop("Trade Date Must be less than settlement date")
    if (missing(method)) 
        method = "ns"
    Rate.Delta = 1
    TermStructure <- TermStructure(trade.date = trade.date, method = method)
    MortgageCashFlow <- MortgageCashFlows(bond.id = bond.id, 
        original.bal = original.bal, settlement.date = settlement.date, 
        price = price, PrepaymentAssumption = PrepaymentAssumption, 
        begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, 
        CPR = CPR)
    MortgageTermStructure <- BondTermStructure(bond.id = MortgageCashFlow, 
        Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
        principal = original.bal * MortgageCashFlow@MBSFactor, 
        price = price, cashflow = MortgageCashFlow)
    new("PassThroughAnalytics", MortgageCashFlow, MortgageTermStructure)
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("PassThroughAnalytics", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (bond.id = "character", 
    original.bal = numeric(), price = numeric(), trade.date = "character", 
    settlement.date = "character", method = method, PrepaymentAssumption = "character", 
    ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), 
    CPR = numeric()) 
{
    if (trade.date > settlement.date) 
        stop("Trade Date Must be less than settlement date")
    if (missing(method)) 
        method = "ns"
    Rate.Delta = 1
    TermStructure <- TermStructure(trade.date = trade.date, method = method)
    MortgageCashFlow <- MortgageCashFlows(bond.id = bond.id, 
        original.bal = original.bal, settlement.date = settlement.date, 
        price = price, PrepaymentAssumption = PrepaymentAssumption, 
        begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, 
        CPR = CPR)
    MortgageTermStructure <- BondTermStructure(bond.id = MortgageCashFlow, 
        Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
        principal = original.bal * MortgageCashFlow@MBSFactor, 
        price = price, cashflow = MortgageCashFlow)
    new("PassThroughAnalytics", MortgageCashFlow, MortgageTermStructure)
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("PassThroughAnalytics", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")))(bond.id, 
    original.bal, price, trade.date, settlement.date, method, 
    PrepaymentAssumption, ..., begin.cpr = begin.cpr, end.cpr = end.cpr, 
    seasoning.period = seasoning.period, CPR = CPR), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mortgage Pass Through }
\keyword{ Analytics }% __ONLY ONE__ keyword per line
