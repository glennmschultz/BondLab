# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#' The Cox, Ingersoll, Ross simulation of the short term forward rate
#' 
#' The function simulates the short-term forward rate according to Cox, Ingersoll, and Ross
#' @param shortrate A numeric value the shortrate
#' @param kappa A numeric value the rate of mean reversion
#' @param theta A numeric value the long term forward rate
#' @param T A numeric value time in years
#' @param step A numeric value the time step
#' @param sigma A numeric value the volatility not annualized
#' @param N the number of paths
#' @export
CIRSim <- function(shortrate = numeric(), 
                   kappa = numeric(), 
                   theta = numeric(), 
                   T = numeric(), 
                   step = numeric(), 
                   sigma = numeric(), 
                   N = numeric()){
  
  #kappa is the rate of mean reversion
  #theta is the long term value of the short rate
  #T is the horizon
  #step is the time between each payment of coupon paying bond
  #N is the number of simulations
  
  #Error Trap Model Parameters
  #if(2*kappa*theta <= sigma^2) 
  #  stop("Invaild parameterization origin is inaccessible")
  
  dt <- step
  nrow <-  T/dt

  deltarate <- function(kappa = numeric(), 
                        theta = numeric(), 
                        dt = numeric(), 
                        sigma = numeric()){
    #Populate the first element of each path with the short rate
    #Euler discretization of the CIR model.  The discretization causes negative interest rates when 
    #when the short term rate approaches the origin.  
    #To solve this problem take the absolute value of square root process  
    (kappa * (theta - simulation[i-1,j]) * dt) + (sigma * sqrt(abs(simulation[i-1,j])) * rnorm(1,0,1))}  
  
  #Matrix to hold the short rate paths - 
  #I can dimnames here rather than colnames same as mortgage oas (rename N to paths?)
  simulation = array(data = 0, c((nrow + 1), N))
  simulation[1,] = shortrate
  
  for(j in 1:N){
    for(i in 2:(nrow + 1)){        
      simulation[i,j] <- simulation[i-1, j] + deltarate(kappa = kappa, 
                                                        theta = theta, 
                                                        dt = dt, 
                                                        sigma = sigma)    
    }
  }
  
  colnames(simulation) <- c(rep((paste("path", seq(1:N)))))
  return(simulation)
}

setGeneric("CIRSim", function(shortrate = numeric(), 
                              kappa = numeric(), 
                              theta = numeric(), 
                              T = numeric(), 
                              step = numeric(), 
                              sigma = numeric(), 
                              N = numeric())
  {standardGeneric("CIRSim")})