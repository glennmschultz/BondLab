
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  #' An S4 class whose slots are functions used to propogate
  #' the mortgage rate used to motivate the prepayment model
  #' @slot yr30 A function defining the 30-year mortgage rate as a function
  #' of the 2- and 10-year swap rate
  #' @slot yr15 A function defining the 15-year mortgage rate as a function
  #' of the 2- and 10-year swap rate 
  #' @export MortgageRate
   
  setClass("MortgageRate",
         representation(
           yr30 = "function",
           yr15 = "function"))

  #' A constructor function for the class Mortgage Rate
  MortgageRate <- function(){
    new("MortgageRate",
        yr30 = function(two = numeric(), ten = numeric(), sato = numeric()) {
                        2.25 + (.06 * two) + (.75 * ten) + sato},
        yr15 = function(two = numeric(), ten = numeric(), sato = numeric()){
                        1.75 + (.06 * two) + (.75 * ten) + sato}
                      )}
  
 