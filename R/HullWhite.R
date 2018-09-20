  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.
  
  #' @include CashFlowMatrix.R
  NULL
  
  #'@title Set Decimal Length
  #'@family General Function
  #'@description A function to set the number of decimal places.  This is used
  #'in the Hull White Model to address the floating point issue that may arise
  #'when simulating interest rates.
  #'@param x the number to be rounded
  #'@param k the number of decimal places
  #'@export DecimalSet
  DecimalSet <- function(x, k) {
    decimal.set = trimws(format(round(x,k), nsmall = k))
    return(as.numeric(decimal.set))
  }
  
  #'@title Zero Bond price
  #'@family Bond Function
  #'@description A function to compute zero coupon bond price
  #'@param r the interest rate used to discount
  #'@param t time time period for discounting
  #'@export ZeroBondPrice
  ZeroBondPrice <- function(r, t){
    bond.price = exp(-r * t)
    return(bond.price)
  }
  
  #'@title Hull White B term
  #'@family Interest Rate Models
  #'@description A function to compute B term of the Hull White Bond Price
  #'@param mean.reversion The mean reversion rate
  #'@param delta.time The time step (maturity of zero coupon bond)
  #'@export Bterm
  Bterm <- function(mean.reversion, 
                    delta.time){
    b.term = (1 - exp(-mean.reversion * delta.time))/mean.reversion
  }
  
  #'@title The single factor Hull White Model used to simulate the short rate
  #'@family Interest Rate Models
  #'@description A function to simulate the short rate using a single factor 
  #'Hull White model.
  #'@param short.rate The short term rate
  #'@param mean.reversion The rate of mean reversion
  #'@param theta The paramter to fit to the curve
  #'@param time The time period in years
  #'@param delta.time The time step
  #'@param volatility The market volatility stated in annual terms
  #'@param trade.days The number of trading days
  #'@param num.paths Then number of short rate paths
  #'@importFrom stats uniroot
  #'@importFrom stats rnorm
  #'@export HullWhite
  HullWhite <- function(short.rate,
                        mean.reversion,
                        theta,
                        time,
                        delta.time,
                        volatility,
                        trade.days,
                        num.paths){
    
    delta.rate <- function(short.rate,
                           mean.reversion,
                           theta,
                           time,
                           delta.time,
                           volatility,
                           trade.days){
      volatility = volatility/sqrt(trade.days)
      delta.rate = ((theta*time) - (mean.reversion *short.rate))*delta.time + (volatility * rnorm(1,0,1))
      delta.rate = DecimalSet(delta.rate, 8)
      return(delta.rate)
    }
    
    num.periods = time/delta.time
    simulation = array(data = 0, c((num.periods+1), num.paths))
    simulation[1,] = short.rate

    for(path in 1:num.paths){
      for(rate in 2:(num.periods + 1)){
        simulation[rate, path] <- simulation[rate -1, path] + 
          delta.rate(short.rate = simulation[rate -1, path],
                     mean.reversion = mean.reversion,
                     theta = theta,
                     time = time,
                     delta.time = delta.time,
                     volatility = volatility,
                     trade.days = trade.days)
      }
    }
    return(simulation)
  }
  
  #'@title Calibrate Hull White to curve
  #'@family Interest Rate Models
  #'@description Calibrates the Hull White model to the current curve