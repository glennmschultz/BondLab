% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpreadToPriceFunctions.R
\name{ZVSpreadToPriceMBS}
\alias{ZVSpreadToPriceMBS}
\title{ZV Spread to price MBS}
\usage{
ZVSpreadToPriceMBS(
  bond.id,
  settlement.date,
  term.structure,
  prepayment.assumption,
  ZV.spread,
  ...,
  CPR = NULL
)
}
\arguments{
\item{bond.id}{a character or connection to object of type BondDetails}

\item{settlement.date}{a character the settlement date 'mm-dd-yyyy'}

\item{term.structure}{a character string referencing an object of type rates data}

\item{prepayment.assumption}{a character string referencing an object of type Prepayment}

\item{ZV.spread}{a character the spread to the spot rate curve quoted in basis points}

\item{..., }{optional values follow}

\item{CPR}{The user may specify a CPR to over riding the prepayment model}
}
\description{
Returns the clean price of an MBS pass throughv via PriceTypes 
object given a spread to the spot rate curve.  Market convention is to quote 
spread to the spot rate curve in basis points.  Bond Lab follows the market 
convection.  The user specified spread to the benchmark in basis points.
}
\seealso{
Other Pricing: 
\code{\link{BenchMark,CurveSpreads-method}},
\code{\link{BillPriceToYield}()},
\code{\link{BillYieldToPrice}()},
\code{\link{CurveSpreads-class}},
\code{\link{CurveSpreads}},
\code{\link{SpreadToBenchmark,CurveSpreads-method}},
\code{\link{SpreadToCurve,CurveSpreads-method}},
\code{\link{SpreadToPriceBond}()},
\code{\link{SpreadToPriceMBS}()},
\code{\link{ZVSpreadToPriceBond}()},
\code{\link{ZeroVolSpread,CurveSpreads-method}}
}
\concept{Pricing}
