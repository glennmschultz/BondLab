
  # script to build FNMA 2016_58 REMIC in Bond Lab

  # A connection to the RAID file to create the RAID from   
  REMICConn <- paste(system.file(package="BondLab"),
                    "/REMICData/2016_053", sep ="")
  source(REMICConn, local = TRUE, echo =FALSE)
  
  REMICConn <- "/Users/glennschultz/Library/R/3.3/library/BondLab/REMICData/2016_053" 
  source(REMICConn)
  
  
  
  