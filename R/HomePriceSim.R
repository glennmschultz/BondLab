

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage 
  # backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.



  setGeneric("HPISim", function(shortrate = numeric(),
                              LongTermGrowth = numeric(),
                              T = numeric(),
                              step = numeric(),
                              sigma = numeric(),
                              N = numeric())
  {standardGeneric("HPISim")})
 
  #' A function to simulate home prices given a short-term rate
  #' 
  #' Simulates home prices as a function of the short-term rate and the 
  #' long term growth of home prices
  #' @param shortrate A numeric value the short term forward rate given 
  #' by the interest rate generation process
  #' @param LongTermGrowth A numeric value the expected long term home 
  #' price growth potential
  #' @param T A numeric value the time in years
  #' @param step A numeric value the time step interval
  #' @param sigma A numeric value the volatility of home price
  #' @param N A numeric value the number of paths
  #' @export
  HPISim <- function(shortrate = numeric(),
                   LongTermGrowth = numeric(),
                   T = numeric(),
                   step = numeric(),
                   sigma = numeric(),
                   N = numeric()){
  # short rate is the short term rate and is passed from the CIR model 
  # to the HIP Model
  # OER is the owner's equivalent rent
  # T is the horizon of the simulation
  # step is the step of the simulation for MBS step is monthly (1/12)
  
  dt <- step
  nrow <- T/dt
                     
  deltahomeprice <- function(shortrate = numeric(),
                             LongTermGrowth = numeric(),
                             step = numeric(),
                             sigma = numeric()){
  round(((.01 * (shortrate - LongTermGrowth)) * dt) + 
      (sigma *  sqrt(abs(homesimulation[i-1,j])) * rnorm(1,0,1)),8)}
  
  homesimulation = array(data = 0, c((nrow + 1), N))
  homesimulation[1,] = 1
  shortrate = ((1 + shortrate) ^(1/12))-1
  LongTermGrowth = ((1 + LongTermGrowth)^(1/12)) - 1

  for(j in 1:N){
    for(i in 2:(nrow + 1)){
      homesimulation[i,j] <- max(0,round(homesimulation[i-1, j] + 
        deltahomeprice(shortrate = shortrate,
                       LongTermGrowth= LongTermGrowth,
                       step = dt,
                       sigma = sigma),8))
    }
  }
  colnames(homesimulation) <- c(rep((paste("path", seq(1:N)))))
  return(homesimulation)
  }

