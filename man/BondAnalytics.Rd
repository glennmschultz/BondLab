\name{BondAnalytics}
\alias{BondAnalytics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BondAnalytics
}
\description{
BondAnalytics is the constructor function for the class BondAnalytics.  It aggregates the results of BondCashFlow and BondTermStructure in the super class BondAnalytics 
}
\usage{
BondAnalytics(bond.id = "character", principal = numeric(), price = numeric(), trade.date = "character", settlement.date = "character", method = method)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bond.id}{
The bond cusip number or psudeo cusip number
}
  \item{principal}{
Principal amount of the bond purchased by the investor
}
  \item{price}{
price is the bond price paid by the investor
}
  \item{trade.date}{
The date on which the trade was initiated and the agreed price set
}
  \item{settlement.date}{
settlement date of the bond
}
  \item{method}{
The method used to fit the term strucutre.  The default is nelson-siegel.  Other methods include severson, dibold-lee or cubic spline. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the class BondAnalytics
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
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
BondAnalytics(bond.id = "bondlab10", principal = 1000, price = 100, trade.date = "01-10-2013", settlement.date = "01-15-2013" )

## The function is currently defined as
structure(function (bond.id = "character", principal = numeric(), 
    price = numeric(), trade.date = "character", settlement.date = "character", 
    method = method) 
{
    standardGeneric("BondAnalytics")
  }, generic = structure("BondAnalytics", package = "BondLab"), package = "BondLab", group = list(), valueClass = character(0), signature = c("bond.id", 
"principal", "price", "trade.date", "settlement.date", "method"
), default = structure(function (bond.id = "character", principal = numeric(), 
    price = numeric(), trade.date = "character", settlement.date = "character", 
    method = method) 
{
    if (missing(method)) 
        method = "ns"
    Rate.Delta = 1
    TermStructure <- TermStructure(trade.date = trade.date, method = method)
    BondCashFlow <- BondCashFlows(bond.id = bond.id, principal = principal, 
        settlement.date = settlement.date, price = price)
    BondTermStructure <- BondTermStructure(bond.id = BondCashFlow, 
        Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
        principal = principal, price = price, cashflow = BondCashFlow)
    new("BondAnalytics", BondCashFlow, BondTermStructure)
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("BondAnalytics", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (bond.id = "character", 
    principal = numeric(), price = numeric(), trade.date = "character", 
    settlement.date = "character", method = method) 
{
    if (missing(method)) 
        method = "ns"
    Rate.Delta = 1
    TermStructure <- TermStructure(trade.date = trade.date, method = method)
    BondCashFlow <- BondCashFlows(bond.id = bond.id, principal = principal, 
        settlement.date = settlement.date, price = price)
    BondTermStructure <- BondTermStructure(bond.id = BondCashFlow, 
        Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
        principal = principal, price = price, cashflow = BondCashFlow)
    new("BondAnalytics", BondCashFlow, BondTermStructure)
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "bond.id", package = "methods"), generic = structure("BondAnalytics", package = "BondLab"), class = structure("derivedDefaultMethod", package = "methods")))(bond.id, 
    principal, price, trade.date, settlement.date, method), class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fixed Income }
\keyword{ Term Structure }% __ONLY ONE__ keyword per line
