
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

  # Swap Rate data creates a data base of daily yield
  # curves using swap rate data from the Federal Reserve

  #' A function to read and convert a .csv data file of swap rates from the 
  #' ICE website
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
  #' SwapRateData(datafile = paste(system.file(package = "BondLab"), 
  #' "/RatesData/20150911.csv", sep = "". header = TRUE),
  #' maturity = c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30))}
  #' @importFrom utils read.csv
  #' @importFrom utils read.delim
  #' @importFrom utils write.csv
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
  #' @export SwapRate
  SwapRate <- function(datafile = "character", maturityvector = numeric()){
    # Note: maturityvector must start with an empty space e.g. c("", 1, 2, )  
    #========== Read Swap Rate Data ===========================
    SwapRateData <-read.csv(datafile, header = TRUE, as.is = TRUE)
    #======== remove month and year data and reorder dataset
    RowCount = nrow(SwapRateData)
    ColCount = ncol(SwapRateData)
    
    for(i in 1:RowCount) {
      if(SwapRateData[i,ColCount] != "ND") {data = SwapRateData[i,]                                      
      data <- rbind(data, as.numeric(maturityvector))
      
     return(data)}
    }
  }
  
  

  #' A function to call swap curve data from the ICE website 
  #' Full swap curve data is reported with a lag of 
  #' approximately four days due to delayed reporting of LIBOR rates by ICE
  #' @param CurveDate a character the swap curve date
  #' @param ... optional value to save curve to rates file
  #' @param save.curve a logical indicating whether to save the rates data TRUE of FALSE
  #' defaults to FALSE
  #' @importFrom RCurl getURL
  #' @importFrom httr GET
  #' @importFrom httr set_cookies
  #' @importFrom httr write_disk

  #' @export GetSwapCurve
  GetSwapCurve <- function(CurveDate = "character", ..., save.curve = FALSE){
  dataSwapURL <- "https://www.theice.com/marketdata/reports/icebenchmarkadmin/ISDAFIXHistoricalRates.shtml"
    dataLiborURL <- "https://www.theice.com/marketdata/reports/icebenchmarkadmin/ICELiborHistoricalRates.shtml"
    SwapPath <- paste(system.file(package = "BondLab"),"/RatesData/Swap.csv", sep = "")
    LiborPath <- paste(system.file(package = "BondLab"),"/RatesData/Libor.csv", sep = "")
    dataRes <- GET(dataSwapURL,
                   query = list(excelExport = "",
                                criteria.reportDate = as.character(CurveDate),
                                criteria.seriesName = I("USD+Rates"),
                                criteria.runCode = "1100"),
                   set_cookies(iceBanner = "rcDisclaimer"), 
                   write_disk(SwapPath, overwrite = TRUE)
    )
    swapdata <- utils::read.csv(SwapPath)
    
    dataRes <- GET(dataLiborURL,
                   query = list(excelExport = "",
                                criteria.reportDate = as.character(CurveDate),
                                criteria.currencyCode = "USD"),
                   set_cookies(iceBanner = "rcDisclaimer"), 
                   write_disk(LiborPath, overwrite = TRUE)
    )
    libordata <- utils::read.csv(LiborPath)
    
    #Dimension SwapRateData array
    SwapData <- matrix(data = NA, nrow = 1, ncol = 12)
    colnames(SwapData) <- c("Date", 
                            "ED1M", 
                            "ED3M", 
                            "ED6M", 
                            "USSW1", 
                            "USSW2", 
                            "USSW3", 
                            "USSW4", 
                            "USSW5", 
                            "USSW7", 
                            "USSW10", 
                            "USSW30")
    
    SwapData[1,1] <- as.character(as.Date(
      as.character(CurveDate), format = "%m-%d-%Y"))
    SwapData[1,2] <- libordata[3,3] #1 month libor rate
    SwapData[1,3] <- libordata[5,3] #3 month libor rate
    SwapData[1,4] <- libordata[6,3] #6 month libor rate
    SwapData[1,5] <- libordata[7,3] #12 month libor rate 
    SwapData[1,6] <- swapdata[2,2]  #2 year swap rate
    SwapData[1,7] <- swapdata[3,2]  #3 year swap rate
    SwapData[1,8] <- swapdata[4,2]  #4 year swap rate
    SwapData[1,9] <- swapdata[5,2]  #5 year swap rate
    SwapData[1,10] <- swapdata[7,2] #7 year swap rate
    SwapData[1,11] <- swapdata[10,2]#10 year swap rate
    SwapData[1,12] <- swapdata[13,2]#30 year swap rate
    
    if(save.curve == FALSE){
      maturity <- c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30)
      utils::write.csv(SwapData, paste(system.file(package = "BondLab"),
                                      "/RatesData/Today.csv", sep = ""),
                      row.names = FALSE)
      SwapCurve <-  try(SwapRate(
        datafile = paste(system.file(package = "BondLab"),
                         "/RatesData/Today.csv", sep = ""), 
        maturityvector = maturity))
      
      return(SwapCurve)
    } else {
      utils::write.csv(SwapData, paste(system.file(package = "BondLab"),
                              "/RatesData/Today.csv", sep = ""),
              row.names = FALSE)
    
    maturity <- c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30)
    try(SwapRateData(datafile = paste(system.file(package = "BondLab"),"/RatesData/Today.csv", sep = ""), 
                     maturityvector = maturity))
    }
  }