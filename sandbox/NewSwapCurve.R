

  library(httr)

  dataSwapURL <- "https://www.theice.com/marketdata/reports/icebenchmarkadmin/ISDAFIXHistoricalRates.shtml"
  dataLiborURL <- "https://www.theice.com/marketdata/reports/icebenchmarkadmin/ICELiborHistoricalRates.shtml"
  SwapPath <- paste(system.file(package = "BondLab"),"/RatesData/Swap.csv", sep = "")
  LiborPath <- paste(system.file(package = "BondLab"),"/RatesData/Libor.csv", sep = "")
  dataRes <- GET(dataSwapURL,
                 query = list(excelExport = "",
                              criteria.reportDate = "12-19-2016",
                              criteria.seriesName = I("USD+Rates"),
                              criteria.runCode = "1100"),
                 set_cookies(iceBanner = "rcDisclaimer"), 
               write_disk(SwapPath, overwrite = TRUE)
  )
  swapdata <- read.csv(SwapPath)
  
  dataRes <- GET(dataLiborURL,
                 query = list(excelExport = "",
                              criteria.reportDate = "12-19-2016",
                              criteria.currencyCode = "USD"),
                 set_cookies(iceBanner = "rcDisclaimer"), 
                 write_disk(LiborPath, overwrite = TRUE)
  )
  libordata <- read.csv(LiborPath)
  
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
  
  SwapData[1,1] <- as.character(as.Date("12-19-2016", format = "%m-%d-%Y"))
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
  
  write.csv(SwapData, paste(system.file(package = "BondLab"),
                            "/RatesData/Today.csv", sep = ""),
            row.names = FALSE)
  
  maturity <- c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30)
  try(SwapRateData(datafile = paste(system.file(package = "BondLab"),"/RatesData/Today.csv", sep = ""), 
                   maturityvector = maturity))