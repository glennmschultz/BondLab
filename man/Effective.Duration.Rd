\name{Effective.Duration}
\alias{Effective.Duration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Effective Duration
}
\description{
Calculated the effective duration of a bond.
 
}
\usage{
Effective.Duration(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rate.Delta}{
The parallel shift in the curve used to calculate the new price of the security 
}
  \item{cashflow}{
The cashflow vector of the bond that is discounted along the spot rate curve
}
  \item{discount.rates}{
The base case spot rate curve used for discouting
}
  \item{discount.rates.up}{
The parallel shift up
}
  \item{discount.rates.dwn}{
The parallel shift down
}
  \item{t.period}{
time period in which the cashflow is received
}
  \item{proceeds}{
Principal times price of the bond
}
}
\details{
Returns the effective duration for a given shift in the spot rate curve
}
\value{
Effective Duration
}
\references{
Investing in Mortgage and Asset Backed Securities with R and Bond Lab (2015), Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
Effective Duration is computed as part of BondTermStrucutre function
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondTermStructure}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Effective Duration }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
