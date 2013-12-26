\name{TermStructure}
\alias{TermStructure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TermStructure
}
\description{
This is a constructure for S4 class object TermStrucutre.  The constructor call the package termstrc
}
\usage{
TermStructure(trade.date = "character", method = "character")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trade.date}{
The date on which the investor (buyer or seller) initiates a trade
}
  \item{method}{
The method used to fit the term strucutre 
}
}
\details{
methods are Nelson-Seigel (default) "ns", Dibold-Lee 
}
\value{
Returns the S4 class TermStructure
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Robert Ferstl, Josef Hayden (2010).,
        Zero-Coupon Yield Curve Estimation with the Package termstrc.,
        Journal of Statistical Software, 36(1), 1-34.,
        URL http://www.jstatsoft.org/v36/i01/.)
        
}
\author{
Glenn Schultz, CFA
}
\note{
The TermStructure class is used to pass the spot and foward rate vectors to BondTermStrucutre and MortgageTermStrucutre
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BondTermStructure}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ term strucutre }
\keyword{ key rate duration }% __ONLY ONE__ keyword per line
\keyword{ effective duration }
\keyword{ effective convexity }