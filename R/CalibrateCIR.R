  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


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
                                               units = c("weeks"))/weeks.in.year,0))
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
  
  #Objective function
  CIRTune <- function(param = numeric(), 
                      shortrate = numeric(), 
                      sigma = .015, 
                      cfmatrix = matrix(), 
                      matmatrix = matrix()){
    
    kappa =  param[1]
    lambda = param[2]
    theta =  param[3]

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
  fit <- optimx(par = c(.1, .003, .03), 
                fn = CIRTune, 
                method = "L-BFGS-B",
                lower = rep(.001,3),
                upper = rep(1, 3), 
                shortrate = shortrate,
                sigma = sigma,
                cfmatrix = CIR.CF.Matrix, 
                matmatrix = CIR.Mat.Matrix)  

  return(fit)
}

setGeneric("CalibrateCIR", function(trade.date = character, 
                                  sigma = numeric())
  {standardGeneric("CalibrateCIR")})