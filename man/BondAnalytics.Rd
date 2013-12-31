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
BondAanlytics extends the classes TermStrucutre, BondTermStructure, and BondCashFlows
}
\value{
Returns the class BondAnalytics
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securities with R and Bond Lab (2015), Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
This class may be further extended in the future
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondCashFlows}},\code{\link{BondBasisConversion}}, \code{\link{BondTermStructure}}
}
\examples{
BondAnalytics(bond.id = "bondlab10", principal = 1000, price = 100, trade.date = "01-10-2013", settlement.date = "01-15-2013", method = "ns")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Fixed Income}
\keyword{Term Structure}
\keyword{Duration}
\keyword{Effective Duration}
\keyword{Key Rate Duration}
\keyword{Convexity}
\keyword{Effective Convexity}
