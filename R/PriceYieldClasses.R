

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.
  

  #' An S4 class representating bond price
  #' 
  #' @slot PriceDecimal A numeric value the price using decimal notation
  #' @slot Price32nds A character the price using 32nds notation
  #' @slot PriceBasis A numeric value price decimal notation in units of 100
  setClass("Price",
           representation(
             PriceDecimal =  "numeric",
             Price32nds = "character",
             PriceBasis = "numeric")
           )