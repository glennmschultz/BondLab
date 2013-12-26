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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Mortgage Pass Through }
\keyword{ Analytics }% __ONLY ONE__ keyword per line
