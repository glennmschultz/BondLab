
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
  #'@family Prepayment and Default
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
  #'@family Prepayment and Default 
  #'@param midpoint The function mid point
  #'@param spread The spread between the note rate and the prevailing mortgage rate
  #'@param max.cpr The function max CPR
  #'@param steepness the function steepness
  #'@export
  BorrowerIncentive <- function(midpoint, max.cpr, steepness, spread){
    BorrowerIncentive = midpoint/(1 + exp(-steepness *(spread - midpoint)))
    return(BorrowerIncentive)
  }
  
  #'@title BurnoutFunction
  #'@description The function is a Markov process of population evolution of fast
  #'and slow payers in a mortgage pool.
  #'@family Prepayment and Default
  #'@param fast.fast The transition probablity of a fast payer to a fast payer in
  #'any given period.  The higher the fast.fast probablity the slower the rate of 
  #'burnout.  Defaults to 0.97
  #'@param slow.fast The transition probability of slow payer to a fast paying in
  #'any given period.  The higher the slow.fast transition the faster the burnout.
  #'Defaults to 0.01 
  #'@param fast.pop The initial estimate of the fast payers in the pool defaults to 0.95
  #'@param slow.pop The initial estimate of the slow payers in the pool defaults to 0.05
  #'@param age The loan or pool age
  #'@export
  BurnoutFunction <- function(fast.fast = .97, slow.fast = .01, fast.pop = .95, slow.pop = .05, age){
    if(age != 1){
      tmatrix <- as.matrix(c(fast.fast, slow.fast))
      tmatrix <- t(tmatrix)
      tmatrix <- rbind(tmatrix, 1-tmatrix[1,])
      colnames(tmatrix) <- c("fast", "slow")
      rownames(tmatrix) <- c("fast", "slow")
      
      popmatrix <- as.matrix(c(fast.pop, slow.pop))
      rownames(popmatrix) <- c("fast", "slow")
      
      for(i in 1:age){
        popmatrix <- tmatrix %*% popmatrix
      }
      
      popmatrix <- tmatrix %*% popmatrix
    } else {
      popmatrix <- tmatrix %*% popmatrix
    }
    return(popmatrix)
  }
  
  #'@title Standard default assumption ramp
  #'@description The function is the conditional default ramp for mortgage 
  #'backed securities.  The function is stylized as the standard default assumption
  #'@family Prepayment and Default
  #'@param BeginCDR The beginning condition default rate
  #'@param PeakCDR The peak conditional default rate
  #'@param EndCDR The ending conditioin default
  #'@param PeakMonth The month in which the conditional default rate reaches it
  #'maximum value on the CDR curve.
  #'@param PlateauMonths The number of months during which the condition default
  #'rate remains at it maximum value.
  #'@param EndMonth The month at which the conditional default rate reaches its
  #'long term rate.
  #'@param LoanAge The age of the loan or pool WALA.
  #'@export
  SDARamp <- function(BeginCDR = numeric(),
                         PeakCDR = numeric(),
                         EndCDR = numeric(),
                         PeakMonth = numeric(),
                         PlateauMonths = numeric(),
                         EndMonth = numeric(),
                         LoanAge = numeric()) 
  {UpRamp = PeakCDR - BeginCDR  
  DownRamp = EndCDR - PeakCDR
  DownRampMonths = EndMonth - (PeakMonth + PlateauMonths)
  PlateauEnd = PeakMonth + PlateauMonths
  ifelse(LoanAge <= PeakMonth, 0 + ((LoanAge-1) * (UpRamp / (PeakMonth - 1))),
         ifelse(LoanAge > PeakMonth & LoanAge <= PlateauEnd ,PeakCDR, 
                ifelse(LoanAge > PlateauEnd & LoanAge <= EndMonth, PeakCDR + (LoanAge - PlateauEnd) *
                         (DownRamp/DownRampMonths),EndCDR)))}
  