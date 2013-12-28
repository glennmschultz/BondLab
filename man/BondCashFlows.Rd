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
BondCashFlows object is used in the BondAnalytics Function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondAnalytics}}, \code{\link{BondBasisConversion}}, \code{\link{BondTermStructure}}
}
\examples{
BondCashFlows(bond.id = "bondlab10", principal = 100000, settlement.date = "01-15-2013", price = 100)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Bond}
\keyword{CashFlow}
\keyword{Modified Duration}
\keyword{Fixed Income}
\keyword{Duration}
\keyword{Convexity}
\keyword{Weighted Average Life}
\keyword{Yield to Maturity}
