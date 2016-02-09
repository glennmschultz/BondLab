  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
 
 
  
  #' Function to assign the beginning principal balance to REMIC tranche
  #'
  #' This function takes the ending balance of the previous period and assigns
  #' the value to the beginning balance in the current period. Structuring functions
  #' run on a tree structure created by the package data.tree.  See data.tree for more details on the package.
  #' @param node The entry point of the deal's tree structure.
  #' @param period A numeric value the period in which payment is recieved stated as an integer.  
  BeginBal <- function(node, period = numeric()){
    if(period == 1) {node$BeginBal[period] <- node$CurrBal
    } else {node$BeginBal[period] <- node$EndingBal[period-1]}}
  
  #' Function to calculate interest in the current period based on Beginning Balance
  #' 
  #' The function takes beginning balance and frequency and period to calculate interest
  #' due the investor in the period.  The coupon is passed via the node argument from the
  #' deal's tree structure.  Structuring functions run on a tree structure created by the 
  #' package data.tree.  See data.tree for more details on the package.
  #' @param node The entry point of the deal's tree structure
  #' @param frequency the number of payments received over the calendar year
  #' @param period A numeric value the period in which payment is received stated as integer
  Interest <- function(node, frequency, period = numeric()){
    node$Interest = node$Coupon/(yield.basis * frequency) * node$BeginBal}
  
  #' Function to assign the ending principal balance to REMIC tranche
  #' 
  #' This function takes the beginning balance of the current period to calculate the 
  #' ending balance of the current period based on the principal paid in the period.  The
  #' principal paid in the period is passed via the node$Principal argument 
  #' Structuring functions run on a tree structure created by the package data.tree
  #' @param node The entry point of the deal's tree structure
  #' @param period A numeric value the period in which payment is received stated as integer
  EndingBal <- function(node, period = numeric()){
    node$EndingBal[period] = as.numeric(node$BeginBal[period]) - as.numeric(node$Principal[period])}
  
 