\name{FutureValue}
\alias{FutureValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FutureValue
}
\description{
Computes the future value of $1 invested today given a discount rate (i), number of periods, and frequency
}
\usage{
FutureValue(interest.rate = numeric(), number.periods = numeric(), frequency = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interest.rate}{
interest.rate interest rate is the annual discount rate used to determine the future value
}
  \item{number.periods}{
number.periods number of periods is the discounting number of periods
}
  \item{frequency}{
frequency The frequency of (i) divides teh annual rate by its frequency.  For example, 1 = annual, 12 = monthly.
}
}
\details{
For example, assume a two year period with semi annual discounting and an annual rate of 5.0\%. i = .05, number of periods = 4, frequency = 2
}
\value{
Returns the future value factor given i, n, and frequency
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securties with R and Bond Lab (2015), Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
Present and future value functions are used in the discounting and valuation of cash flows
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PresentValue}},\code{\link{PresentValueAnnuity}}
}
\examples{
FutureValue(.05, number.periods = 4, frequency = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Present Value }
\keyword{ Present Value Annuity }
\keyword{ Fixed Income }
