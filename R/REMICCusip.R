
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

  #' A function to move REMIC tranche cusip data to the Bond Data Director
  #' 
  #' This function moves REMIC tranche data from the Tranches data folder which is used
  #' by the REMIC constructor to the BondData folder.  This is the final step after the 
  #' REMIC structure has been validated.
  #' @param source.path A charachter string the source path to the Traches folder
  #' @param target.path A character string the target path to the BondData folder
  #' @export CusipRecord
  CusipRecord <- function(source.path = "character", target.path = "character"){
  cusip.list <- list.files(source.path)
  count <- length(cusip.list)

  for(i in 1 : count){
    file.name <- cusip.list[i]
    temp <- readRDS(paste(as.character(source.path), as.character(file.name), sep =""))
    
    connCusip <- gzfile(description = paste(as.character(target.path), as.character(temp@Cusip), ".rds", sep = ""))
    saveRDS(temp, connCusip)

  }
    
    on.exit(close.connection(connCusip))
  } 