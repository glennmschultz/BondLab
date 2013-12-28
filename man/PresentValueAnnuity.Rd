\name{PresentValueAnnuity}
\alias{PresentValueAnnuity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PresentValueAnnuity
}
\description{
Computes the present value of a \$1 annuity given a discount rate (i), number of periods, adn frequency
}
\usage{
PresentValueAnnuity(interest.rate = numeric(), number.periods = numeric(), frequency = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interest.rate}{
interest rate is the annual discount rate used to determine the present value
}
  \item{number.periods}{
number of periods is the discounting number of periods
}
  \item{frequency}{
The frequency of (i) divides the annual rate by its frequency. For example, 1 = annual, 12 = monthly
}
}
\details{
for example, assume a two year period with semi-annual discounting and an annual rate of 5.0\%, i =.05, number of periods = 4, freqency = 2
}
\value{
The present value factor given an interest rate (i), number of periods (n), and frequency (i)
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
Present Value of Annuity used in the discounting and valuation of cash flows
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PresentValue}}, \code{\link{FutureValue}},\code{\link{FutureValueAnnuity}},
}
\examples{
PresentValueAnnuity(i = .05, number.periods = 4, frequency = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fixed Income }
\keyword{ Present Value }
