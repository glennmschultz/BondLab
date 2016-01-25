  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.

  RollingDelinquecy <- function(DelinquecyVector = numeric(), numperiods = numeric){
    x = NULL
    rowSums(outer(1:(length(DelinquecyVector)-numperiods+1),1:numperiods,FUN=function(i,j){x[(j - 1) + i]}))
  }