\name{TimeValue}
\alias{TimeValue}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time Value
%%  ~~function to do ... ~~
}
\description{
Function is used to compute the time value (present and future value) factors used to discount cash flows.
}
\usage{
TimeValue(interest.rate = numeric(), number.periods = numeric(), frequency = numeric(), type = "character")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interest.rate}{
Interest rate is entered as follows 0.03 to represent 3.0%
}
  \item{number.periods}{
The number of years is entered as follows 5 to represent 5-years.
}
  \item{frequency}{
The number of periods in one-year.  For example, 12 means monthly payments or 12 discounting periods in one-year
}
  \item{type}{
Entered as a charcter string; Future Value = "FV", Present Value = "PV", Future Value Annuity "FVA", Present Value Annuity "PVA". 
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
 TimeValue(.03, 5, 12, "FV")

## The function is currently defined as
function (interest.rate = numeric(), number.periods = numeric(), 
    frequency = numeric(), type = "character") 
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
    if (!type \%in\% c("PV", "PVA", "PVAD", "FV", "FVA")) 
        stop("Time value function not specfied correctly")
    interest.rate = interest.rate/frequency
    switch(type, PV = 1/(1 + interest.rate)^number.periods, PVA = ((1 - 
        (1/(1 + interest.rate)^number.periods))/interest.rate), 
        PVAD = ((1 - (1/(1 + interest.rate)^number.periods))/interest.rate) * 
            (1 + interest.rate), FV = (1 + interest.rate)^number.periods, 
        FVA = (((1 + interest.rate)^(number.periods)) - 1)/interest.rate)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
