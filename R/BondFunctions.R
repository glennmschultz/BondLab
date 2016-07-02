
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.

  setGeneric("bondprice", function(yield.to.maturity = numeric(),
                                 coupon = numeric(), 
                                 coupon.frequency = numeric(), 
                                 years.mat = numeric(), 
                                 face.value = numeric())
  {standardGeneric("bondprice")})

  #' Determine the price a bond on the interest payment date 
  #' 
  #' This is generic function to determine the price of a bond
  #' given the yield to maturity (YTM).  It is a nominal example of 
  #' pricing a bond given its yield to maturity.  The equation assumes 
  #' pricing from one payment date to the next.  It does not account for
  #' acrrued interest.
  #' @param yield.to.maturity A numeric value expressing the yield to
  #' maturity (discount rate) as an annual percentage.
  #' @param coupon A numeric value the coupon paid by the bond as a 
  #' percentage of
  #' the bond's principal amount
  #' @param coupon.frequency A numeirc value expressing the frequency 
  #' of payments over one year
  #' @param years.mat A numeric value expressing the years to maturity
  #' @param face.value A numeric value expressing the face value 
  #' (principal amount) of the bond
  #' @examples bondprice(
  #' yield.to.maturity = .05, coupon = .05, coupon.frequency = 2,
  #' years.mat = 10, face.value = 100)
  #' @export bondprice
  bondprice<- function(yield.to.maturity = numeric(),
                     coupon = numeric(), 
                     coupon.frequency = numeric(), 
                     years.mat = numeric(), 
                     face.value = numeric()){
  
  n = years.mat * coupon.frequency
  c = coupon/coupon.frequency
  i = yield.to.maturity/coupon.frequency
  fv = face.value
  
  (((1-(1/(1+i)^n))/i) * (fv * c)) + (1/(1+i)^n * fv)
  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("EstimYTM", function(coupon = numeric(), 
                                  coupon.frequency = numeric(), 
                                  years.mat = numeric(), 
                                  face.value = numeric(), 
                                  price = numeric())
  {standardGeneric("EstimYTM")})
  
  #' A function to estimate the yield to maturity of a standard bond
  #' 
  #' Estimate a bond's yield to maturity given a price.  It is a 
  #' nominall example and does not account for acrrued interest
  #' @param coupon A numeric value expressing the bond's coupon as a
  #' percentage of its face amount
  #' @param coupon.frequency A numeric value expressing the frequency of payments
  #' over one year
  #' @param years.mat A numeric value expressing the years to maturity
  #' @param face.value A numeric value expressing the face value (principal amount) of the bond
  #' @param price A numeric value expressing the price of the bond (not percentage of face value)
  #' for example a price of$102 is entered as 102.00
  #' @examples EstimYTM(coupon = .04, 
  #' coupon.frequency = 2, years.mat = 10, face.value = 1000, price = 100)
  #' @export EstimYTM
  EstimYTM <- function(coupon = numeric(), 
                     coupon.frequency = numeric(), 
                     years.mat = numeric(), 
                     face.value = numeric(), 
                     price = numeric()){
  c = coupon
  n = years.mat
  f = coupon.frequency
  fv = face.value
  p = price/price.basis 
  ((c * fv) + ((fv - (fv *p))/2)) / (((fv + (fv *p))/f))
  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("Effective.Duration", function(Rate.Delta = numeric(), 
                                            cashflow = vector(), 
                                            discount.rates = vector(), 
                                            time.period = vector())
  {standardGeneric("Effective.Duration")})

  #' A function to compute effective duration
  #' 
  #' Calculates the effective duration based on dscount vector (zero coupon)
  #' cashflow vector, and rate delta
  #' @param Rate.Delta A numeric value the interest rate shift in basis points
  #' @param cashflow A numeric vector of cashflow
  #' @param discount.rates A numeric vector of the discount rates
  #' @param time.period A numeric vector of the time period
    #' @export
  Effective.Duration <- function(Rate.Delta = numeric(), 
                               cashflow = vector(), 
                               discount.rates = vector(), 
                               time.period = vector()){

  discount.rates.up = discount.rates + Rate.Delta
  discount.rates.dwn = discount.rates - Rate.Delta
  Price.NC = sum((1/((1+discount.rates)^time.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^time.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^time.period)) * cashflow)  
  (Price.UP - Price.DWN)/(2*Price.NC*Rate.Delta)
  }
  
  #-----------------------------------------------------------------------------------------------
  setGeneric("Effective.Convexity", function(Rate.Delta = numeric(), 
                                             cashflow = vector(), 
                                             discount.rates = vector(), 
                                             time.period = vector())
  {standardGeneric("Effective.Convexity")})

  #' A function to compute effective convexity
  #' 
  #' Calculates effective convexity based on a discount vector (zero coupon)
  #' cashflow vector, and rate delta
  #' @param Rate.Delta  A numeric value the interest rate shift in basis points
  #' @param cashflow A numeric vector of cashflow
  #' @param discount.rates A numeric vector of the up discount rates
  #' @param time.period A numeric vector of the down discount rates
  #' @export Effective.Convexity
  Effective.Convexity <- function(Rate.Delta = numeric(), 
                                 cashflow = vector(), 
                                 discount.rates = vector(),
                                 time.period = vector()){
  discount.rates.up = discount.rates + Rate.Delta
  discount.rates.dwn = discount.rates - Rate.Delta
  Price.NC = sum((1/((1+discount.rates)^time.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^time.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^time.period)) * cashflow)
  
  (Price.UP + Price.DWN - (2*Price.NC))/(2*Price.NC*(Rate.Delta^2))
  }
  
  #-----------------------------------------------------------------------------------------------
  setGeneric("Forward.Rate", function(SpotRate.Curve = vector(),
                                      FwdRate.Tenor = numeric())
  {standardGeneric("Forward.Rate")})


  #' A function to calculate forward rates
  #' 
  #' Calculate forward rate given a vector of spot rates
  #' @param SpotRate.Curve A vector of monthly spot rates
  #' @param FwdRate.Tenor A numeric value the tenor of the forward rate in months
  #' @export Forward.Rate
  Forward.Rate <- function(SpotRate.Curve = vector(),
                           FwdRate.Tenor){
                max.maturity <- length(SpotRate.Curve)
                num.period <- seq(from = 1/months.in.year, to = max.maturity/months.in.year, by = 1/months.in.year)
                FutureValueVector <- (1 + SpotRate.Curve) ^ num.period
                
                Forward.Rate <- FutureValueVector[(FwdRate.Tenor + 1):max.maturity] / 
                                FutureValueVector[1 : (max.maturity - (FwdRate.Tenor + 0))]
                Forward.Rate <- (Forward.Rate ^ (1/(FwdRate.Tenor/months.in.year)))-1
  }

  