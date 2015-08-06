#' A function to compute effective duration and convexity
#' 
#' Calculates the effective duration and based on discount vector (zero coupon)
#' cashflow vector, and rate delta
#' @param Rate.Delta A numeric value the interest rate shift in basis points
#' @param cashflow A numeric vector of cashflow
#' @param discount.rates A numeric vector of the discount rates
#' @param time.period A numeric vector of the time period
#' @param type A character vector to specify either duration or convexity
#' @export
Effective.Measure <- function(Rate.Delta = numeric(), 
                               cashflow = vector(), 
                               discount.rates = vector(), 
                               time.period = vector(),
                               type = "character"){
  
  discount.rates.up = discount.rates + Rate.Delta
  discount.rates.dwn = discount.rates - Rate.Delta
  Price.NC = sum((1/((1+discount.rates)^time.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^time.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^time.period)) * cashflow)
  
  switch(type,
         duration =   (Price.UP - Price.DWN)/(2*Price.NC*Rate.Delta),
         convexity =  (Price.UP + Price.DWN - (2*Price.NC))/(2*Price.NC*(Rate.Delta^2)))

}

setGeneric("Effective.Measure", function(Rate.Delta = numeric(), 
                                          cashflow = vector(), 
                                          discount.rates = vector(), 
                                          time.period = vector(),
                                          type = "character")
{standardGeneric("Effective.Measure")})
