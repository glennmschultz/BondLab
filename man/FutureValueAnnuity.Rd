\name{FutureValueAnnuity}
\alias{FutureValueAnnuity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Future Value of Annuity
}
\description{
Computes the future value factor of $1 annuity (paid or received) given a discount rate (i), number of periods (n), and frequency
}
\usage{
FutureValueAnnuity(interest.rate = numeric(), number.periods = numeric(), frequency = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interest.rate}{
The annual discount rate used to determine the future value of the annuity}
  \item{number.periods}{
The number of periods used in discounting
}
  \item{frequency}{
Divides the annual rate by its frequency.  For example, 1 = annual, 12 = monthly,
}
}
\details{
For example, assume a two year period with semi annual discounting and an annual rate of 5.0\%. i = .05, number of periods = 4, frequency = 2
}
\value{
Future value factor given i, n, and frequency
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Investing in Mortgage and Asset Backed Securities with R and Bond Lab (2105), Glenn Schultz, CFA
}
\author{
Glenn Schultz, CFA
}
\note{
Present and future value functions are used in the discounting and valuation of cash flows
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{FutureValue}}, \code{\link{PresentValue}}, \code{\link{PresentValueAnnuity}},
}
\examples{
FutureValueAnnuity(.05, number.periods = 4, frequency = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Present Value }
\keyword{ Future Value }
\keyword{ Present Value Annuity}
\keyword{ Future Value Annuity}

