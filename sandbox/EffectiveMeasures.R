#-------------------------------------
#Risk measures.  
#These functions measure effective duration, effective convexity, and key rate duration
#-------------------------------------

Effective.Duration <- function(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds){
  Price = proceeds/10
  Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)  
  (Price.UP - Price.DWN)/(2*Price*Rate.Delta)
}

Effective.Convexity <- function(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds){
  Price = proceeds/10
  Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
  
  (Price.UP + Price.DWN + (2*Price))/(2*(Price*Rate.Delta)^2)
}

