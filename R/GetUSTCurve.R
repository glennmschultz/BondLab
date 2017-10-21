
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.
  
  #--------------------------------------------------------------------------
  # Helper Functions These function help to manage The data sources   
  #-------------------------------------------------------------------------

  # UST Rate data creates a data base of daily yield
  # curves using UST rate data from the Federal Reserve

  #' A function to read and convert a .csv data file of UST rates from the 
  #' St. Lousi Federal Reserve website
  #' 
  #' The function converts a .csv file of rates data to yield curve objects 
  #' that can be read by the Term Structure wapper
  #' @param datafile A ccharacter vector the path to the .csv of rates data 
  #' for see RatesData.csv in the RateData folder for the proper strucutre
  #' @param maturityvector A numeric vector maturities corresponding to the 
  #' tenors in RateData.csv.  Note the maturity 
  #' vector must start with an empty space e.g. c("", 1, 2, ).  Set the working 
  #' directory to the directory holding the 
  #' rates data
  #' @examples
  #' \dontrun{
  #' USTRateData(datafile = paste(system.file(package = "BondLab"), 
  #' "/RatesData/20150911.csv", sep = "". header = TRUE),
  #' maturity = c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30))}
  #' @importFrom utils read.csv
  #' @importFrom utils read.delim
  #' @importFrom utils write.csv
  #' @export USTRateData
  USTRateData <- function(datafile = "character", maturityvector = numeric()){
  # Note: maturityvector must start with an empty space e.g. c("", 1, 2, )  
  #========== Read UST Rate Data ===========================
  USTRateData <-read.csv(datafile, header = TRUE, as.is = TRUE)
  #======== remove month and year data and reorder dataset
  RowCount = nrow(USTRateData)
  ColCount = ncol(USTRateData)
  
  for(i in 1:RowCount) {
    if(USTRateData[i,ColCount] != "ND") {data = USTRateData[i,]                                      
    data <- rbind(data, as.numeric(maturityvector))
    
    saveRDS(data, paste(
      system.file(package = "BondLab"),"/RatesData/", 
      as.character(data[1,1]), ".rds", sep = ""), compress = TRUE)}}
  }
  
  # Swap Rate data creates a data base of daily yield
  # curves using swap rate data from the Federal Reserve
  
  #' A function to read and convert a .csv data file of swap rates from the 
  #' Federal Reseve to rates data
  #' 
  #' The function converts a .csv file of rates data to yield curve objects 
  #' that can be read by the Term Structure wapper
  #' @param datafile A ccharacter vector the path to the .csv of rates data 
  #' for see RatesData.csv in the RateData folder for the proper strucutre
  #' @param maturityvector A numeric vector maturities corresponding to the 
  #' tenors in RateData.csv.  Note the maturity 
  #' vector must start with an empty space e.g. c("", 1, 2, ).  Set the working 
  #' directory to the directory holding the 
  #' rates data
  #' @examples
  #' \dontrun{
  #' SwapRate(datafile = paste(system.file(package = "BondLab"), 
  #' "/RatesData/20150911.csv", sep = "". header = TRUE),
  #' maturity = c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30))}
  #' @importFrom utils read.csv
  #' @importFrom utils read.delim
  #' @importFrom utils write.csv
  #' @export USTRate
  USTRate <- function(datafile = "character", maturityvector = numeric()){
    # Note: maturityvector must start with an empty space e.g. c("", 1, 2, )  
    #========== Read Swap Rate Data ===========================
    USTRateData <-read.csv(datafile, header = TRUE, as.is = TRUE)
    #======== remove month and year data and reorder dataset
    RowCount = nrow(USTRateData)
    ColCount = ncol(USTRateData)
    
    for(i in 1:RowCount) {
      if(USTRateData[i,ColCount] != "ND") {data = USTRateData[i,]                                      
      data <- rbind(data, as.numeric(maturityvector))
      
     return(data)}
    }
  }
  
  

  #' A function to call UST constant maturity curve data from the St, Louis Fed  
  #' FRED website.
  #' @param CurveDate a character the UST curve date
  #' @param FREDAPI a character the user's FRED API Key
  #' @param ... optional value to save curve to rates file
  #' @param save.curve a logical indicating whether to save the rates data TRUE of FALSE
  #' defaults to FALSE
  #' @importFrom RCurl getURL
  #' @importFrom lubridate days
  #' @importFrom XML xmlParse
  #' @importFrom XML xpathSApply
  #' @importFrom XML xmlGetAttr
  #' @importFrom httr GET
  #' @importFrom httr set_cookies
  #' @importFrom httr write_disk

  #' @export GetUSTCurve
  GetUSTCurve <- function(CurveDate, FREDAPI, ..., save.curve = FALSE){
    APIKey <- FREDAPI
    rates <- list("DGS1MO", 
                  "DGS3MO", 
                  "DGS6MO",
                  "DGS1",
                  "DGS2", 
                  "DGS3", 
                  "DGS5", 
                  "DGS7",
                  "DGS10", 
                  "DGS20", 
                  "DGS30")
    
    RateData <- matrix(data = NA, nrow = 1, ncol = 12)
    colnames(RateData) <- c("Date", 
                            "1MO", 
                            "3MO", 
                            "6MO", 
                            "12MO", 
                            "2YR", 
                            "3YR", 
                            "5YR", 
                            "7YR", 
                            "10YR", 
                            "20YR", 
                            "30YR")
    
    startdate <- as.character(as.Date(
      as.character(CurveDate), format = "%m-%d-%Y") + days(0))
    enddate <- as.character(
      as.Date(as.character(CurveDate), format = "%m-%d-%Y"))
    RateData[1,1] = startdate 
    
    for(i in 1:11){
      
      USTRate <- getURL(
        paste("https://api.stlouisfed.org/fred/series/observations?series_id=",
              as.character(rates[i]),
              "&observation_start=", startdate,
              "&observation_end=", enddate, 
              "&api_key=", APIKey,
              "&file_type=xml", sep =""))
      
      doc = xmlParse(USTRate)
      RateValue <- as.numeric(
        xpathSApply(doc, "//observation", xmlGetAttr, "value"))
      RateData[1,as.numeric(i) +1] = RateValue}
    
    if(save.curve == FALSE){
      maturity <- c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30)
      utils::write.csv(RateData, paste(system.file(package = "BondLab"),
                                      "/RatesData/Today.csv", sep = ""),
                      row.names = FALSE)
      USTCurve <-  try(USTRate(
        datafile = paste(system.file(package = "BondLab"),
                         "/RatesData/Today.csv", sep = ""), 
        maturityvector = maturity))
      
      return(USTCurve)
    } else {
      utils::write.csv(RateData, paste(system.file(package = "BondLab"),
                              "/RatesData/Today.csv", sep = ""),
              row.names = FALSE)
    
    maturity <- c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30)
    try(USTRateData(datafile = paste(system.file(package = "BondLab"),"/RatesData/Today.csv", sep = ""), 
                     maturityvector = maturity))
    }
  }