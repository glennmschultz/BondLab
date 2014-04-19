\name{DollarRollAnalytics}
\alias{DollarRollAnalytics}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dollar Roll Analysis
%%  ~~function to do ... ~~
}
\description{
The dollar roll function calculates the value of the one month dollar roll using the function Dollar Roll
}
\usage{
DollarRollAnalytics(bond.id = "character", original.bal = numeric(), price = numeric(), drop = numeric(), trade.date = "character", settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(), finance.rate = numeric(), method = "ns", PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bond.id}{
%%     ~~Describe \code{bond.id} here~~
}
  \item{original.bal}{
%%     ~~Describe \code{original.bal} here~~
}
  \item{price}{
%%     ~~Describe \code{price} here~~
}
  \item{drop}{
%%     ~~Describe \code{drop} here~~
}
  \item{trade.date}{
%%     ~~Describe \code{trade.date} here~~
}
  \item{settlement.date}{
%%     ~~Describe \code{settlement.date} here~~
}
  \item{fwd.settlement.date}{
%%     ~~Describe \code{fwd.settlement.date} here~~
}
  \item{reinvestment.rate}{
%%     ~~Describe \code{reinvestment.rate} here~~
}
  \item{finance.rate}{
%%     ~~Describe \code{finance.rate} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{PrepaymentAssumption}{
%%     ~~Describe \code{PrepaymentAssumption} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{begin.cpr}{
%%     ~~Describe \code{begin.cpr} here~~
}
  \item{end.cpr}{
%%     ~~Describe \code{end.cpr} here~~
}
  \item{seasoning.period}{
%%     ~~Describe \code{seasoning.period} here~~
}
  \item{CPR}{
%%     ~~Describe \code{CPR} here~~
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
DollarRollAnalytics(bond.id = "bondlabMBS55", trade.date = "01-10-2013", 
        settlement.date = "01-14-2013", fwd.settlement.date = "02-12-2013", 
        original.bal = 1250000, price = 102.84375, reinvestment.rate = 0.21/100, 
        finance.rate = 0.31/100, drop = .375, PrepaymentAssumption = "CPR", CPR = 30)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
