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
PassThroughAnalytics(bond.id = "character", original.bal = numeric(), price = numeric(), trade.date = "character", settlement.date = "character", method = method, scenario.set = vector(), PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
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

 \item{scenario.set}{
 The scenario classes called by name for each interest rate scenario the user would like to run on each pass
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
Returns the superclass PassThrough Analytics
}
\value{
Superclass
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securities (2015) with R and Bond Lab, Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
The class may be extended in the future
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MortgageCashFlows}}, \code{\link{TermStructure}}, \code{\link{BondTermStructure}}
}
\examples{
PassThroughAnalytics(bond.id = "bondlabMBS4", original.bal = 100000, price = 100, trade.date = "01-10-2013", 
                     settlement.date = "01-13-2013",  scenario.set = c("UA100", "UA150"),
                     PrepaymentAssumption = "CPR", CPR = 6)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mortgage Pass Through }
\keyword{ Analytics }
\keyword{ Duration }
\keyword{ Effective Duration }
\keyword{Effective Convexity }
\keyword{ Key Rate Duration }
\keyword{ Weighted Average Life}
\keyword{ Yield to Maturity }

