
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


  #' A function to call swap curve data from the St. Louis Federal Reserve
  #' (FRED) database.  Full swap curve data is reported with a lag of 
  #' approximately four days due to delayed reporting of LIBOR rates by ICE
  #' @importFrom RCurl getURL
  #' @importFrom lubridate days
  #' @importFrom XML xmlParse 
  #' @importFrom XML xmlGetAttr
  #' @importFrom XML xpathSApply
  #' @param CurveDate a character the swap curve date
  #' @param APIKey a character the user API Key provided by FRED
  #' @export GetSwapCurve
  GetSwapCurve <- function(CurveDate = "character",
                         APIKey = "character"){
  rates <- list("USD1MTD156N", 
                "USD3MTD156N", 
                "USD6MTD156N",
                "USD12MD156N",
                "DSWP2", 
                "DSWP3", 
                "DSWP4", 
                "DSWP5",
                "DSWP7", 
                "DSWP10", 
                "DSWP30")
  
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
  
  startdate <- as.character(as.Date(
    as.character(CurveDate), format = "%m-%d-%Y") + days(0))
  enddate <- as.character(
    as.Date(as.character(CurveDate), format = "%m-%d-%Y"))
  SwapData[1,1] = startdate 
  
  for(i in 1:11){
    
    LIBORRate <- getURL(
      paste("https://api.stlouisfed.org/fred/series/observations?series_id=",
            as.character(rates[i]),
            #"&realtime_start=",startdate, 
            #"&realtime_end=", enddate,
            "&observation_start=", startdate,
            "&observation_end=", enddate, 
            "&api_key=", APIKey,
            "&file_type=xml", sep =""))
    doc = xmlParse(LIBORRate)
    RateValue <- as.numeric(
      xpathSApply(doc, "//observation", xmlGetAttr, "value"))
    SwapData[1,as.numeric(i) +1] = RateValue}
  
  write.csv(SwapData, paste(system.file(package = "BondLab"),
                            "/RatesData/Today.csv", sep = ""),
            row.names = FALSE)
  
  maturity <- c("",.0833, .25, .5, 1, 2, 3, 4, 5, 7, 10, 30)
  try(SwapRateData(datafile = paste(system.file(package = "BondLab"),"/RatesData/Today.csv", sep = ""), 
                   maturityvector = maturity))}
