  
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

  #' A function to calcualte the cashflow of the REMIC collateral
  #' 
  #' Calculate the REMIC collateral cashflow.  This function is used to
  #' structure a REMIC at the pricing speed.  This function is also used to 
  #' reverse engineer a REMIC
  #' @param bond.id a character the bond cusip or id
  #' @param settlement.date a character the settlement date
  #' @param term.structure a character refering to object of type TermStructure
  #' @param principal a character the current balance contributed to the REMIC
  #' @param PSA the PSA assumtion used to price the deal
  #' @export CollateralCashFlow
  CollateralCashFlow <- function(bond.id = "character", 
                                 settlement.date = "character",
                                 term.structure = "character",
                                 principal = numeric(),
                                 PSA = numeric()){
    
    MBS <- MBS(MBS.id = bond.id)
    TermStructure <- term.structure
    MortgageRate <- MtgRate()
    ModelTune <- ModelTune(MBS)
    
    Vector <- PrepaymentModel(
      bond.id = MBS,
      TermStructure = TermStructure,
      MortgageRate = MortgageRate,
      ModelTune = ModelTune,
      Burnout = BurnOut(MBS),
      PrepaymentAssumption = "PPC",
      begin.cpr = .002 * (PSA/PSA.basis),
      end.cpr = .06 * (PSA/PSA.basis),
      seasoning.period = 30)
    
    CollateralCashFlow <- CashFlowEngine(
      bond.id = MBS,
      settlement.date = settlement.date,
      principal = principal,
      PrepaymentAssumption = Vector)}
  
  #' Function to assign the beginning principal balance to REMIC tranche
  #'
  #' This function takes the ending balance of the previous period and assigns
  #' the value to the beginning balance in the current period. Structuring 
  #' functions run on a tree structure created by the package data.tree.  
  #' See data.tree for more details on the package.
  #' @param node The entry point of the deal's tree structure.
  #' @param period A numeric value the period in which payment is recieved 
  #' stated as an integer.
  #' @export BeginBal  
  BeginBal <- function(node, period){
    if(period == 1) {node$BeginBal[period] <- node$CurrBal
    } else {node$BeginBal[period] <- node$EndingBal[period-1]}}
  
  #' Function to calculate interest in the current period based on Beginning 
  #' Balance.
  #' 
  #' The function takes beginning balance and frequency and period to 
  #' calculate interest due the investor in the period.  The coupon is passed 
  #' via the node argument from the deal's tree structure.  Structuring 
  #' functions run on a tree structure created by the 
  #' package data.tree.  See data.tree for more details on the package.
  #' @param node The entry point of the deal's tree structure
  #' @param frequency the number of payments received over the calendar year
  #' @param period A numeric value the period in which payment is received 
  #' stated as integer
  Interest <- function(node, frequency, period = numeric()){
    node$Interest = node$Coupon/(yield.basis * frequency) * node$BeginBal}
  
  #' Function to assign the ending principal balance to REMIC tranche
  #' 
  #' This function takes the beginning balance of the current period to 
  #' calculate the ending balance of the current period based on the principal 
  #' paid in the period.  The principal paid in the period is passed via the 
  #' node$Principal argument Structuring functions run on a tree structure 
  #' created by the package data.tree
  #' @param node The entry point of the deal's tree structure
  #' @param period A numeric value the period in which payment is received 
  #' stated as integer
  EndingBal <- function(node, period = numeric()){
    node$EndingBal[period] = as.numeric(node$BeginBal[period]) - 
      as.numeric(node$Principal[period])}

  #' A function to calculate the floater coupon 
  #' 
  #' The function calculates the floater coupon given
  #' @param cap a numeric value the floater cap
  #' @param floor a numeric value the floater floor
  #' @param margin a numeric value the floater margin
  #' @param index a numeric value the index value
  #' @param multiplier a numeric value the index multiplier
  #' @export FloatCoupon
  FloatCoupon <- function(cap, floor, margin, index, multiplier){
    min(cap, max((index * multiplier) + margin, floor))}