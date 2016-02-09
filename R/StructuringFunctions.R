  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
 
 
  # ====================================================================
  # Cash flow functions to determine the beginning bal, ending bal, and 
  # interest payment to each tranche/bond.  
  # ====================================================================
  
  #' Function to assign the beginning principal balance to REMIC tranche
  #'
  #' This function takes the ending balance of the previous period and assigns
  #' the value to the beginning balance in the current period
  #' @param node 
  BeginBal <- function(node, period = numeric()){
    if(period == 1) {node$BeginBal[period] <- node$CurrBal
    } else {node$BeginBal[period] <- node$EndingBal[period-1]}}
  
  Interest <- function(node, period = numeric()){
    node$Interest = node$Coupon/(100 * 12) * node$BeginBal}
  
  EndingBal <- function(node, period = numeric()){
    node$EndingBal[period] = as.numeric(node$BeginBal[period]) - (as.numeric(node$Senior_Prin[period]) + as.numeric(node$Sub_Prin[period]))}
  
 