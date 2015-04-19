# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 
#--------------------------------------


#' Calculate the price of a zero coupon bond using the closed form equation given by Cox, Ingersoll, and Ross
#' 
#' This function implements the CIR closed form solution to a bond price given the short-term rate
#' @param shortrate A vector of simulated short-term rates
#' @param T A numeric value the time over which the short-term rate is simulated
#' @param step A numeric value the time step on the simulation
#' @param kappa A numeric value the speed of mean reversion of the simulation
#' @param lambda A numeric value the risk premium
#' @param sigma A numeric value the volatility entered as a standard deviation
#' @param theta A numeric vlaue the long term forward rate
#' @param result A character string the desired output either "p" - Price, "y" - Yield, or "l" - Limit
#' @export  
CIRBondPrice <- function(shortrate = vector(), 
                         T = numeric(), 
                         step = numeric(), 
                         kappa = numeric(), 
                         lambda = numeric(), 
                         sigma = numeric(), 
                         theta = numeric(), 
                         result = character){
  #Error trap the function
  
  if (missing(shortrate))
    stop("Need to specify shortrate.")
  
  # This error throws a warning in OAS
  #if (shortrate < 0 | shortrate > 1)
  #  stop("No valid interest.rate specified.")
  
  if (missing(T))
    stop("Need to specify maturity.")
  
  if (missing(step))
    stop("Need to specify step.")
  
  if (missing(kappa))
    stop("Need to specify kappa.")
  
  if (kappa < 0 | kappa > 1)
    stop("No valid kappa specified.")
  
  if (missing(lambda))
    stop("Need to specify lambda")
  
  if (lambda < 0 | lambda > 1)
    stop("No valid lambda specified.")
  
  if (missing(sigma))
    stop("Need to specify sigma")
  
  if (sigma < 0 | sigma > 1)
    stop("No valid sigma specified.")
  
  if (missing(theta))
    stop("Need to specify theta")
  
  if (theta < 0 | theta > 1)
    stop("No valid theta specified.")
  
  if(missing(result))
    result = "p"  
  
  #T is the maturity is the zero coupon bond. To price a coupon paying bond this is the maturity of the bond
  #step the time between each payment of a coupon paying bond
  
  #kappa is the rate of mean reversion  
  #lambda is the market risk premium (lambda must be negative)
  #sigma is interest rate variance - specificlly sigma^2
  #theta is the mean interest rate level
  #T is the vector of maturities of the yield curve each cash flow is treated as a zero coupon bond  
  #T = c(seq(from = step, to = T, by = step))
  
  T = if(step != 0) {c(seq(from = step, to = T, by = step))} else {T}
  
  #t is the start period this is the first step of the simulation as well as the step size
  #for example from t = 0 to the next period is 0 + step
  t = step
  
  #The below are the functions to determine the closed form solution to the CIR Model
  gamma = ((kappa + lambda)^2 + (2 * (sigma^2))) ^ (1/2)
  
  A.CIR = ((2 * gamma * exp(((kappa + lambda + gamma) * (T-t))/2)) 
           /
             ((gamma + lambda + kappa) * (exp(gamma * (T-t)) - 1) + (2 * gamma))) ^ ((2 * kappa * theta)/(sigma^2))
  
  B.CIR = ((2 * (exp(gamma * (T-t)) - 1))
           /
             ((gamma + lambda + kappa) * (exp(gamma * (T-t)) - 1) + (2 * gamma)))
  
  Price = A.CIR * exp((B.CIR * -1) * shortrate) 
  Yield = (shortrate * B.CIR  - log(A.CIR))/(T-t)
  Limit = (2* kappa* theta) /(gamma + kappa + lambda)
  
  CIRBondPrice = switch(result,
                        p = Price,
                        y = Yield,
                        l = Limit)  
  
}

setGeneric("CIRBondPrice", function(shortrate = vector(), 
                                  T = numeric(), 
                                  step = numeric(), 
                                  kappa = numeric(), 
                                  lambda = numeric(), 
                                  sigma = numeric(), 
                                  theta = numeric(), 
                                  result = character)
  {standardGeneric("CIRBondPrice")})