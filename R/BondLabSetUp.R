  
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.

  #' Setup function for the R package BondLab to create the needed objects
  #' 
  #' @export
  BondLabSetUp <- function(){
    
    do.call(source, 
            list(file = paste(system.file(package = "BondLab"), "/Scenario/Scenario_SpotCurve", sep = ""),
            local = TRUE))
    do.call(source,
            list(file = paste(system.file(package = "BondLab"), "/Scenario/Scenario_YieldCurve", sep = ""),
                 local = TRUE))
  }