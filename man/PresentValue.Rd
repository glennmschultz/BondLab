\name{PresentValue}
\alias{PresentValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Present Value
}
\description{
Computes the present value of $1 in the future given a discount rate (i), number of periods, and frequency
}
\usage{
PresentValue(interest.rate = numeric(), number.periods = numeric(), frequency = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interest.rate}{
interest rate is the annual discount rate used to determine the present value
}
  \item{number.periods}{
number of periods is the discounting numer of periods
}
  \item{frequency}{
The frequency of (i)  Divides the annual rate by it frequency.  For example 1 = annual, 12 = monthly
}
}
\details{
For example, assume a two year period with semi-annual discounting and an annual rate of 5.0\%.  i = .05, number of periods = 4, frequency = 2
}
\value{
Returns the present value factor
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securities using R and Bond Lab (2015), Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
Present value to discount cashflows
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PresentValueAnnuity}}, \code{\link{FutureValueAnnuity}}, \code{\link{FutureValue}}
}
\examples{
PresentValue(.05, number.periods = 4, frequency = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Fixed Income }
\keyword{ Present Value }% 

