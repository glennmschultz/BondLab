% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelToCPR.R
\name{ModelToCPR}
\alias{ModelToCPR}
\title{Converts a mortgage prepayment model vector to a CPR measure}
\usage{
ModelToCPR(
  bond.id,
  settlement.date,
  term.structure,
  original.bal,
  price,
  yield
)
}
\arguments{
\item{bond.id}{A character string referring to an object of type
MBSDetails.}

\item{settlement.date}{A character string the settlement date.}

\item{term.structure}{A character string referring to an object of type}

\item{original.bal}{A numeric value the original balance.}

\item{price}{A character string the price.}

\item{yield}{A numeric value the yield to maturity given a prepayment
model vector}
}
\description{
The function ties the model vector yield to an equivalent CPR yield using
secant method of interpolation.
}
