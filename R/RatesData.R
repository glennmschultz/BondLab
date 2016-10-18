
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