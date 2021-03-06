% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BondFunctions.R
\name{BondEffectiveDuration}
\alias{BondEffectiveDuration}
\title{A function to compute effective duration}
\usage{
BondEffectiveDuration(
  Rate.Delta = numeric(),
  cashflow = vector(),
  discount.rates = vector(),
  time.period = vector()
)
}
\arguments{
\item{Rate.Delta}{A numeric value the interest rate shift in basis points}

\item{cashflow}{A numeric vector of cashflow}

\item{discount.rates}{A numeric vector of the discount rates}

\item{time.period}{A numeric vector of the time period}
}
\description{
Calculates the effective duration based on dscount vector (zero coupon)
cashflow vector, and rate delta
}
