% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpreadToPriceFunctions.R
\name{ZVSpreadToPriceBond}
\alias{ZVSpreadToPriceBond}
\title{ZV Spread to Price Bond}
\usage{
ZVSpreadToPriceBond(bond.id, settlement.date, term.structure, ZV.spread)
}
\arguments{
\item{bond.id}{a character or connection to object of type BondDetails}

\item{settlement.date}{a character the settlement date 'mm-dd-yyyy'}

\item{term.structure}{a character string referencing a term structure object}

\item{ZV.spread}{a character the spread to the spot rate curve quoted in basis points}
}
\description{
returns the clean price of a bond via PriceTypes object given a 
spread to the spot rate curve.  Market convention is to quote spread to a 
the spot rate curve in basis points.  Bond Lab follows the market convection.  The 
user specified spread to the benchmark in basis points.
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
\code{\link{ZVSpreadToPriceMBS}()},
\code{\link{ZeroVolSpread,CurveSpreads-method}}
}
\concept{Pricing}
