#Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


# This function creates cusip files for the Bond Data Directory
# Used with the REMIC constructure once the tranches are established in the Tranche directory
CusipRecord <- function(source.path = "character", target.path = "character"){
  
  cusip.list <- list.files(source.path)
  count <- length(cusip.list)

  for(i in 1 : count){
    file.name <- cusip.list[i]
    temp <- readRDS(paste(as.character(source.path), as.character(file.name), sep =""))
    
    connCusip <- gzfile(description = paste(as.character(target.path), as.character(temp@Cusip), ".rds", sep = ""))
    saveRDS(temp, connCusip)

  }
    
    close(connCusip)
} 