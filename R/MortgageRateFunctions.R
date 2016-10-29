
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
  
 