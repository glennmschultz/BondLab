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
MortgageCashFlows class is the CashFlow input to BondTermStrucuture
}
\value{
Returns the class MortgageCashFlows
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securities with R and BondLab (2015), Glenn Schultz
}
\author{
Glenn Schultz, CFA
}
\note{
Extends the class BondTermStructure and superclass PassThroughAnalytics
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondTermStructure}}, \code{\link{PassThroughAnalytics}}, \code{\link{BondBasisConversion}}
}
\examples{
MortgageCashFlows(bond.id = "bondlabMBS4", original.bal = 100000, settlement.date = "01-13-2013", price = 100, PrepaymentAssumption = "PPC", begin.cpr = .2, end.cpr = 6, seasoning.period = 30) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Duration }
\keyword{ Modified Duration }
\keyword{ Yield to Maturity }
\keyword{ Prepayment }
\keyword{ Weighted Average Life }
