# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


#----------------------------------
# Helper Functions These function help to manage The data sources   
#----------------------------------

  # Swap Rate data creates a data base of daily yield
  # curves using swap rate data from the Federal Reserve

  #' A function to read and convert a .csv data file of swap rates from the Federal Reseve to rates data
  #' 
  #' The function converts a .csv file of rates data to yield curve objects that can be read by the Term Structure wapper
  #' @param datafile A .csv of rates data see RatesData.csv in the RateData folder for the proper strucutre
  #' @param maturityvector A numeric vector maturities corresponding to the tenors in RateData.csv.  Note the maturity 
  #' vector must start with an empty space e.g. c("", 1, 2, ).  Set the working directory to the directory holding the 
  #' rates data
  #' @examples
  #' \dontrun{
  #' SwapRateData(datafile = "RatesData.csv", 
  #' maturity = c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30))}
  #' @export SwapRateData
  SwapRateData <- function(datafile = "character", maturityvector = numeric()){
  # Note: maturityvector must start with an empty space e.g. c("", 1, 2, )  
  #========== Read Swap Rate Data ===========================
  SwapRateData <-read.csv(datafile, header = TRUE, as.is = TRUE)
  #======== remove month and year data and reorder dataset
  RowCount = nrow(SwapRateData)
  ColCount = ncol(SwapRateData)
  
  for(i in 1:RowCount) {
    if(SwapRateData[i,ColCount] != "ND") {data = SwapRateData[i,]                                      
                                          data <- rbind(data, as.numeric(maturityvector))
                                          saveRDS(data, paste(data[1,1], ".rds", sep = ""), compress = TRUE)}}
}