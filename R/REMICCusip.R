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