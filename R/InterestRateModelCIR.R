# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA

  setGeneric("CIRSim", function(shortrate = numeric(), 
                              kappa = numeric(), 
                              theta = numeric(), 
                              T = numeric(), 
                              step = numeric(), 
                              sigma = numeric(), 
                              N = numeric())
  {standardGeneric("CIRSim")})

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
    #Still at high volatility the model occasionally produces negative interest rates
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


  setGeneric("CIRBondPrice", function(shortrate = vector(), 
                                      T = numeric(), 
                                      step = numeric(), 
                                      kappa = numeric(), 
                                      lambda = numeric(), 
                                      sigma = numeric(), 
                                      theta = numeric(), 
                                      result = character)
  {standardGeneric("CIRBondPrice")})

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
  
  # This error throws a warning in OAS if the short rate is nominally negative this is the foating point proble
  #if (shortrate < 0 | shortrate > 1)
  #  stop("No valid interest.rate specified.")
  
  if (missing(T))
    stop("Need to specify maturity.")
  
  if (missing(step))
    stop("Need to specify step.")
  
  if (missing(kappa))
    stop("Need to specify kappa.")
  
  if (kappa < 0)
    stop("No valid kappa specified.")
  
  if (missing(lambda))
    stop("Need to specify lambda")
  
  if (lambda < 0)
    stop("No valid lambda specified.")
  
  if (missing(sigma))
    stop("Need to specify sigma")
  
  if (sigma < 0)
    stop("No valid sigma specified.")
  
  if (missing(theta))
    stop("Need to specify theta")
  
  if (theta < 0 )
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



  setGeneric("CalibrateCIR", function(trade.date = character, 
                                      sigma = numeric())
  {standardGeneric("CalibrateCIR")})
  
  #---------------------------------
  # Calibrate CIR to market
  #________________________________
  #' A function to calibrate the Cox, Ingersoll, Ross single factor model
  #' 
  #' The function calibrates the CIR Model to daily swap curve data
  #' @importFrom lubridate %m+%
  #' @importFrom termstrc create_cashflows_matrix create_maturities_matrix
  #' @import optimx
  #' @param trade.date A numeric value the trade date
  #' @param sigma the volatility in absolute percent the annualized value is sigma times sqrt(240)
  #' @export  
  CalibrateCIR <- function(trade.date = character, 
                         sigma = numeric()){
  
  rates.data <- Rates(trade.date = trade.date)  
  shortrate = as.numeric(rates.data[1,2])/100
  
  #set the column counter to make cashflows for termstrucutre
  ColCount <- as.numeric(ncol(rates.data))
  Mat.Years <- as.numeric(rates.data[2,2:ColCount])
  Coupon.Rate <- as.numeric(rates.data[1,2:ColCount])
  Issue.Date <- as.Date(rates.data[1,1])
  
  #initialize coupon bonds S3 class
  #This can be upgraded when bondlab has portfolio function
  ISIN <- vector()
  MATURITYDATE <- vector()
  ISSUEDATE <- vector()
  COUPONRATE <- vector()
  PRICE <- vector()
  ACCRUED <- vector()
  CFISIN <- vector()
  CF <- vector()
  DATE <- vector()
  CASHFLOWS  <- list(CFISIN,CF,DATE)
  names(CASHFLOWS) <- c("ISIN","CF","DATE")
  TODAY <- vector()
  data <- list()
  TSInput <- list()
  
  ### Assign Values to List Items #########
  data = NULL
  data$ISIN <- colnames(rates.data[2:ColCount])
  data$ISSUEDATE <- rep(as.Date(rates.data[1,1]),ColCount - 1)
  
  data$MATURITYDATE <-
    sapply(Mat.Years, function(Mat.Years = Mat.Years, Issue = Issue.Date) 
    {Maturity = if(Mat.Years < 1) {Issue %m+% months(round(Mat.Years * months.in.year))} else 
    {Issue %m+% years(as.numeric(Mat.Years))}
    return(as.character(Maturity))
    }) 
  
  data$COUPONRATE <- ifelse(Mat.Years < 1, 0, Coupon.Rate)                  
  
  #data$PRICE <- rep(100, ColCount -1)
  data$PRICE <- ifelse(Mat.Years < 1, (1 + (Coupon.Rate/100))^(Mat.Years * -1) * 100, 100)
  
  data$ACCRUED <- rep(0, ColCount -1)
  
  for(j in 1:(ColCount-1)){
    Vector.Length <- as.numeric(round(difftime(data[[3]][j],
                                               data[[2]][j],
                                               units = c("weeks")),0)/weeks.in.year)
    Vector.Length <- ifelse(Vector.Length < 1, 1, Vector.Length * pmt.frequency)  #pmt.frequency should be input 
    data$CASHFLOWS$ISIN <- append(data$CASHFLOWS$ISIN, rep(data[[1]][j],Vector.Length))
    data$CASHFLOWS$CF <- append(data$CASHFLOWS$CF,as.numeric(c(rep((data[[4]][j]/100/pmt.frequency),Vector.Length-1) * min.principal, (min.principal + (data$COUPONRATE[j]/100/pmt.frequency)* min.principal))))
    by.months = ifelse(data[[4]][j] == 0, round(difftime(data[[3]][j], rates.data[1,1])/days.in.month), 6) # this sets the month increment so that cashflows can handle discount bills
    data$CASHFLOWS$DATE <- append(data$CASHFLOW$DATE,seq(as.Date(rates.data[1,1]) %m+% months(as.numeric(by.months)), as.Date(data[[3]][j]), by = as.character(paste(by.months, "months", sep = " "))))
    
  } #The Loop Ends here and the list is made
  
  data$TODAY <- as.Date(rates.data[1,1])
  TSInput[[as.character(rates.data[1,1])]] <- c(data)
  
  #set term strucutre input (TSInput) to class couponbonds
  class(TSInput) <- "couponbonds"
  CashFlow <- TSInput[[1]]
  CIR.CF.Matrix <- create_cashflows_matrix(TSInput[[1]], include_price = TRUE)
  CIR.Mat.Matrix <- create_maturities_matrix(TSInput[[1]], include_price = TRUE )
  
  #Objective function for the origin to be inaccessable the followign condition must be met
  # 2 * kappa * theta <= sigma^2  
  CIRTune <- function(param = numeric(), 
                      shortrate = numeric(), 
                      sigma = sigma, 
                      cfmatrix = matrix(), 
                      matmatrix = matrix()){
    
    kappa =  param[1]
    lambda = param[2]
    theta =  param[3]
    
    # gamma masked as lambda per Ben Bolker email
    #lambda = (lambda + sigma^2)/(2 * kappa) 
    
    Disc <- CIRBondPrice(kappa = kappa, 
                         lambda = lambda, 
                         theta = theta, 
                         shortrate = shortrate, 
                         T= matmatrix,  
                         step = 0, 
                         sigma = sigma)
    
    CIRTune <- sqrt((sum(colSums((cfmatrix * Disc))^2))/ncol(matmatrix))
    return(CIRTune)
  }
  
  # Fit the model to the market   
  fit <- optimx(par = c(.05, .05, .01), 
                fn = CIRTune, 
                method = "L-BFGS-B",
                lower = c(.01, .01, .01),
                upper = c(.5, .1, .2) , 
                shortrate = shortrate,
                sigma = sigma,
                cfmatrix = CIR.CF.Matrix, 
                matmatrix = CIR.Mat.Matrix)  
  
  return(fit)
  }

  
