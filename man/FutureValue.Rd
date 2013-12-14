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
Glenn Schultz, CFA
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
FutureValue(.05, number.periods = 4, frequency = 2)

## The function is currently defined as
function (interest.rate = numeric(), number.periods = numeric(), 
    frequency = numeric()) 
{
    if (missing(interest.rate)) 
        stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
    if (!is.numeric(interest.rate)) 
        stop("No numeric interest.rate specified.")
    if (interest.rate < 0 | interest.rate > 1) 
        stop("No valid  interest.rate specified.")
    if (missing(number.periods)) 
        stop("Need to provide the number of discounting periods")
    if (!is.numeric(number.periods)) 
        stop(" No numeric discounting period specfified")
    if (number.periods < 1) 
        stop("No valid  number.periods specified.")
    if (missing(frequency)) 
        stop("Need to provide the frequency")
    if (!is.numeric(frequency)) 
        stop(" No numeric frequency specfified")
    if (frequency < 1 | frequency > 12) 
        stop("No valid frequency specified.")
    interest.rate = interest.rate/frequency
    (1 + interest.rate)^number.periods
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
