
  # script to build FNMA 2016_58 REMIC in Bond Lab

  # A connection to the RAID file to create the RAID from   
  REMICConn <- paste(system.file(package="BondLab"),
                    "/REMICData/2016_053", sep ="")
  source(REMICConn, local = TRUE, echo =FALSE)
  
  SCHEDConn <- paste(system.file(package="BondLab"),
                     "/REMICData/2016_053_Schedules.txt", sep ="")
  
  schedule <- readLines(SCHEDConn)
  schedule <- schedule[c(seq(-8, -1, 1))]
  schedule <- sub(",", "", schedule)
  schedule <- gsub("\\.{12}","", schedule)
  schedule <- gsub("\\.{11}","", schedule)
  schedule <- gsub("\\.{10}","", schedule)
  schedule <- gsub("\\.{9}","", schedule)
  schedule <- gsub("\\.{8}","", schedule)
  schedule <- gsub("\\.{7}","", schedule)
  schedule <- gsub("\\.{6}","", schedule)
  schedule <- gsub("\\.{5}","", schedule)
  schedule <- gsub("\\.{4}","", schedule)
  schedule <- gsub("\\.{3}","", schedule)
  schedule <- gsub("\\.{2}","", schedule)
  schedule <- gsub("^[:space:] ", ",", schedule)
 
  SCHEDG1Conn <- paste(system.file(package="BondLab"),
                     "/REMICData/2016_053_Schedules_G1.txt", sep ="")
 write.table(schedule, file = SCHEDG1Conn, row.names = FALSE, sep = " ")
 
 Schedules<- read.csv(SCHEDG1Conn,row.names = NULL)
  