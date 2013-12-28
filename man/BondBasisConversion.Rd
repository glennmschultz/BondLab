\name{BondBasisConversion}
\alias{BondBasisConversion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bond Basis Conversion
}
\description{
This function is used to adjust the bond basis.  The bond basis is the payment date count used to compute interest due 
}
\usage{
BondBasisConversion(issue.date, start.date, end.date, settlement.date, lastpmt.date, nextpmt.date, coupon, principal, frequency, price)
}

\arguments{
  \item{issue.date}{
The issue date of the bond
}
  \item{start.date}{
The issue dated date which is the date at which interest begins to accrue
}
  \item{end.date}{
The maturity date of the bond
}
  \item{settlement.date}{
The date the bond settles between seller and buyer
}
  \item{lastpmt.date}{
The previous interest payment date (often referred to as the last payment date)
}
  \item{nextpmt.date}{
The upcoming interest payment date
}
  \item{coupon}{
The coupon (interest rate) paid to the investor
}
  \item{principal}{
The principal balance outstanding
}
  \item{frequency}{
The number of payments made to the investor
}
  \item{price}{
The price paid by the buyer to the seller
}
}
\details{
This function is used to compute acrrued interest and days between payment dates based on the
the bond basis (day count convention used to compute interest due the investor)
}
\value{
Returns fraction of the year remaining for the calculation of acrrued interest
}
\references{
Investing in Mortgage and Asset Backed Securities Using R and Bond Lab (2015), Glenn Schultz
}
\author{
Glenn Schultz, CFA
}
\note{
Used in BondCashFlows and MortgageCashFlows
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondCashFlows}}, \code{\link{MortgageCashFlows}}, \code{\link{BondAnalytics}}, \code{\link{PassThroughAnalytics}}
}
\examples{

BondBasisConversion(issue.date = as.Date(c("2013-01-15")), start.date = as.Date(c("2013-01-15")), end.date = as.Date(c("2015-01-15")), settlement.date = as.Date(c("2013-01-15")), lastpmt.date = as.Date(c("2013-01-15")), nextpmt.date = as.Date(c("2013-07-15")), coupon = .03, principal = 1000, frequency = 2, price = 100)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Basis }
\keyword{ Fixed Income }% __ONLY ONE__ keyword per line
