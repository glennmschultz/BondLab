\name{BondTermStructure}
\alias{BondTermStructure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BondTermStructure
}
\description{
This function calculates the effective duration, effective convexity, and key rate duration of a fixed income security or a portfolio of fixed income securities 
}
\usage{
BondTermStructure(bond.id = "character", Rate.Delta = numeric(), TermStructure = "character", principal = numeric(), price = numeric(), cashflow = "character")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bond.id}{
The cusip or psudeo cusip number
}
  \item{Rate.Delta}{
The change in rates used to calcualte effective duration and convexity recommended rate delta is 1
}
  \item{TermStructure}{
The TermStructure object passed to BondTermStrucuture
}
  \item{principal}{
The outstanding principal balance of the bond or orginal face amount of MBS
}
  \item{price}{
The price paid to the seller by the buyer
}
  \item{cashflow}{
The cashflow object passed to TermStrucutre.  
}
}
\details{
Requires BondCashFlow or MortgageCashFlow object and TermStructure object
}
\value{
Returns Classs BondTermStrucutre
%%  If it is a LIST, use
\item{SpotSpread }{Constant spread added to the spot rate curve for discounting bond yield}
\item{Effective Duration }{The sum of the key rate durations}
\item{Effective Convexity}{The sum of the key rate convexities}
\item{Key Rate Tenor}{The tenor (key rate) on the spot rate curve}
\item{Key Rate Duration}{The duration of the key rate}
\item{Key Rate Convexity}{The convexity of the key rate}
}
\references{
Investing in Mortgage and Asset Backed Securities with R and Bond Lab (2015), Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
BondTermStrucutre is dependent on input from classes BondCashFlows or MortgageCashFlows, and TermStructure
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondCashFlows}}, \code{\link{MortgageCashFlows}}, \code{\link{TermStructure}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Key Rate Duration }
\keyword{ Effective Duration }% __ONLY ONE__ keyword per line
\keyword{ Effective Convexity }
\keyword{ Term Strucuture }