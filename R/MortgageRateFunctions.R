
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
  
  #' A standard generic function to access the slot yr30 from class MortgageRate
  #' @param object an S4 class object
  #' @export yr30 
  setGeneric("yr30", function(object)
    {standardGeneric("yr30")})
  
  #' A standard generic function to access the slot yr15 from class MortgageRate
  #' @param object an S4 class object
  #' @export yr15
  setGeneric("yr15", function(object)
    {standardGeneric("yr15")})
  
  #' A constructor function for the class Mortgage Rate
  #' @param two.year A numeric value the two year rate
  #' @param ten.year A numeric value the ten year rate
  #' @param sato A numeric value the borrower SATO
  #' @export
  MortgageRate <- function(two.year, ten.year, sato){
    new("MortgageRate",
        yr30 = function(two.year = two.year, ten,year = ten.year, sato = sato) {
                        2.25 + (.06 * two.year) + (.75 * ten.year) + sato},
        yr15 = function(two.year = two.year, ten.year = ten.year, sato = sato){
                        1.75 + (.06 * two.year) + (.75 * ten.year) + sato}
                      )}
  
 