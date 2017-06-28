
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

  #' @include PrepaymentModelTune.R
  NULL

  #'@title Seasonal Factor 
  #'@description The function creates seasonal factors for use in prepayment 
  #'modeling
  #'@param alpha numeric value the function alpha
  #'@param month numeric value the month of year
  #'@param theta a numeric value the theta
  #'@export
  Seasonals <- function(alpha = numeric(), 
                        month = numeric(), 
                        theta = numeric())
  {seasonal.factor =(1  + alpha *sin((pi/2 * (month + theta - 3)) / 3 - 1))
  return(seasonal.factor)}
  
  #'@title BorrowerIncentive
  #'@description The function is the logit used to model incentive.  The 
  #'function may be used to fit incentive curve based on either wac spread to 
  #'the mortgage rate or annuity ratio. 
  #'@param midpoint The function mid point
  #'@param spread The spread between the note rate and the prevailing mortgage rate
  #'@param max.cpr The function max CPR
  #'@param steepness the function steepness
  #'@export
  BorrowerIncentive <- function(midpoint, max.cpr, steepness, spread){
    BorrowerIncentive = midpoint/(1 + exp(-steepness *(spread - midpoint)))
    return(BorrowerIncentive)
  }