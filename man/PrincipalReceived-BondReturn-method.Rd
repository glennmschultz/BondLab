% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BondScenario.R
\name{PrincipalReceived,BondReturn-method}
\alias{PrincipalReceived,BondReturn-method}
\title{PrincipalReceived method, class BondReturn}
\usage{
\S4method{PrincipalReceived}{BondReturn}(object)
}
\arguments{
\item{object}{An S4 class of type BondReturn}
}
\description{
A method to get \strong{Principal Received} over the 
scenario horizon to the investor.  Principal Received is reported 
as the sum of scheduled principal received by the investor.
}
\seealso{
Other Bond Scenario Analysis: 
\code{\link{BondReturn-class}},
\code{\link{BondScenario-class}},
\code{\link{BondScenario}},
\code{\link{CouponIncome,BondReturn-method}},
\code{\link{HorizonCurrBal,BondReturn-method}},
\code{\link{HorizonMos,BondReturn-method}},
\code{\link{HorizonPrice,BondReturn-method}},
\code{\link{HorizonReturn,BondReturn-method}},
\code{\link{ReinvestmentIncome,BondReturn-method}},
\code{\link{ScenarioHorizonMos,ScenarioCurve-method}}
}
\concept{Bond Scenario Analysis}
