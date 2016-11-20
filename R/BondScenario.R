
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
  
  #' @include ScenarioConstructor.R BondCashFlows.R TermStructure.R
  #' @include BondTermStructure.R MortgageScenario.R CurveSpreads.R
  NULL
  
  #' An S4 class a list of bond scenarios for analysis
  #' 
  #' The class BondScenarioSet is a list of classes of the type scenario.  The
  #' class is used to facilitate multiple scenario analysis by providing a user
  #' defined list for scenario analysis
  #' @slot ScenarioSet A list of the classes of the type scenario
  #' @exportClass BondScenarioSet
  setClass("BondScenarioSet",
           representation(
             ScenarioSet = "list"
           ))
  
  setGeneric("BondScenarioSet", function(object)
             {standardGeneric("BondScenarioSet")})
  
  # Note: standardGeneric ScenarioSet is found in MortgageScenario.R
  
  setMethod("initialize",
            signature("BondScenarioSet"),
            function(.Object,
                     ScenarioSet = "list",
                     ...)
            {
              callNextMethod(.Object,
                            ScenarioSet = ScenarioSet,
                            ...)
            })

  #' A Method to extract the slot Scenario from the object BondScenarioSet
  #' 
  #' @param object The name of an S4 object of the type BondScenarioSet
  #' @exportMethod ScenarioSet
  setMethod("ScenarioSet", signature("BondScenarioSet"),
            function(object){object@ScenarioSet})
  
  #' BondScenarioSet is a constructor function for the class BondScenarioSet
  #' 
  #' @param ScenarioSet a list of the scenario classes used for analysis
  #' @export BondScenarioSet
  BondScenarioSet <- function(ScenarioSet = "list"){
    new("BondScenarioSet",
        ScenarioSet = ScenarioSet)
  }
  
  #' An S4 class the results of bond total return scenario analysis
  #' 
  #' The class BondReturn holds the results of bond total return analysis
  #' @slot CouponIncome A numeric value the coupon income received over the 
  #' investment horizon
  #' @slot PrincipalReceived A numeric value the scheduled principal 
  #' received over the investment horizon
  #' @slot ReinvestmentIncome A numeric value the reivestment income received 
  #' over the investment horizon
  #' @slot HorizonCurrBal A numeric value the current balance at the end of the
  #' investment horizon
  #' @slot HorizonPrice A numeric the price at the end of the horizon
  #' @slot HorizonReturn A numeric value the horizon total return
  #' @slot HorizonMos A numeric value the number of months to 
  #' the scenario horizon date
  #' @exportClass BondReturn
  setClass("BondReturn",
           representation(
             CouponIncome = "numeric",
             PrincipalReceived = "numeric",
             ReinvestmentIncome = "numeric",
             HorizonCurrBal = "numeric",
             HorizonPrice = "numeric",
             HorizonReturn = "numeric",
             HorizonMos = "numeric"))
  
  # Note: standardGeneric CouponIncome is found in MortgageScenario.R
  
  # ' A standard generic function to access the slot PrincipalRecevied
  setGeneric("PrincipalReceived", function(object)
    {standardGeneric("PrincipalReceived")})
  
  # Note: standardGeneric ReinvestmentIncome is found in MortgageScenario.R
  # Note: HorizonCurrBal is found in MortgageScenario.R
  # Note: HorizonPrice is found in MortgageScenario.R
  # Note: HorizonReturn is found in MortgageScenario.R
  # Note: HorizonMos is found in MortgageScenario.R

  setMethod("initialize",
            signature("BondReturn"),
            function(.Object,
                     CouponIncome = numeric(),
                     PrincipalReceived = numeric(),
                     ReinvestmentIncome = numeric(),
                     HorizonCurrBal = numeric(),
                     HorizonReturn = numeric(),
                     HorizonMos = numeric(),
                     ...)
            {
              callNextMethod(.Object,
                             CouponIncome = CouponIncome,
                             PrincipalReceived = PrincipalReceived,
                             ReinvestmentIncome = ReinvestmentIncome,
                             HorizonCurrBal = HorizonCurrBal,
                             HorizonReturn = HorizonReturn,
                             HorizonMos = HorizonMos)
            })
  #' A method to extract CouponIncome from S4 class BondReturn
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod CouponIncome
  setMethod("CouponIncome", signature("BondReturn"),
            function(object){object@CouponIncome})
  
  #' A method to extract ScheduledPrinReceived from S4 class BondReturn
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod ScheduledPrinReceived
  setMethod("PrincipalReceived", signature("BondReturn"),
            function(object){object@PrincipalReceived})
  
  #' A method to extract ReinvestmentIncome from S4 class BondReturn
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod ReinvestmentIncome
  setMethod("ReinvestmentIncome", signature("BondReturn"),
            function(object){object@ReinvestmentIncome})
  
  #' A method to extract HorizonCurrBal from S4 class BondReturn
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod HorizonCurrBal
  setMethod("HorizonCurrBal", signature("BondReturn"),
            function(object){object@HorizonCurrBal})
  
  #' A method to extract HorizonPrice from S4 class BondReturn
  #' @param object the name of an S4 class of tyoe BondReturn
  #' @exportMethod HorizonPrice
  setMethod("HorizonPrice", signature("BondReturn"),
            function(object){object@HorizonPrice})
  
  #' A method to extract HorizonReturn from S4 class BondReturn
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod HorizonReturn
  setMethod("HorizonReturn", signature("BondReturn"),
            function(object){object@HorizonReturn})
  
  #' A method to extract HorizonMos from S4 class BondReturn
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod HorizonMos
  setMethod("HorizonMos", signature("BondReturn"),
            function(object){object@HorizonMos})
  
  #' An S4 class the bond return scenario analysis
  #' 
  #' The SuperClass BondScenario holds the results of a scenario analysis run
  #' BondScenario contains the following classes: TermStructure, BondCashFlow,
  #' BondTermStructure, BondReturn, CurveSpreads, and Scenario.  BondScenario
  #' inherts the getters and setters of the above classes.
  #' @exportClass BondScenario
  setClass("BondScenario",
           representation(),
           contains = c("TermStructure",
                        "BondCashFlows",
                        "BondTermStructure",
                        "BondReturn",
                        "CurveSpreads",
                        "Scenario"))
  
  setGeneric("BondScenario", function(bond.id = "character",
                                      settlement.date = "character",
                                      rates.data = "character",
                                      scenario = "character",
                                      horizon.months = "character",
                                      method = "character",
                                      ...,
                                      horizon.spot.spread = NULL,
                                      horizon.nominal.spread = NULL,
                                      horizon.OAS = NULL,
                                      horizon.price = NULL)
    {standardGeneric("BondScenario")})
  
  #' Bond Scenario Analysis
  #' 
  #' A function to compute the total return of a Bond
  #' @param bond.id A character string referencing an object of the type BondDetails
  #' @param settlement.date A character string the settlement data "mm-dd-YYYY".
  #' @param rates.data A character string the trade.data "mm-dd-YYYY"
  #' @param price A character string in decimal equivalent (.) or 32nds (-)
  #' @param par.amount A numeric value the par amount. 
  #' @param scenario A character string the scenario
  #' @param horizon.months A numeric value the time horizon
  #' @param method A character string the method used to fit the term structure
  #' @param ... Optional values to select term structure and horizon price method
  #' @param horizon.spot.spread A numeric value the horizon zero volatility 
  #' spread
  #' @param horizon.nominal.spread A numeric value the horizon nominal spread
  #' or spread to  the curve
  #' @param horizon.OAS A numeric value the horizon option adjusted spread
  #' (not currently implemented)
  #' @param horizon.price A numeric value the horizon price
  #' @export BondScenario
  BondScenario <- function(bond.id = "character",
                           settlement.date = "character",
                           rates.data = "character",
                           price = numeric(),
                           par.amount = numeric(),
                           scenario = "character",
                           horizon.months = numeric(),
                           method = "ns",
                           ...,
                           horizon.spot.spread = NULL,
                           horizon.nominal.spread = NULL,
                           horizon.OAS = NULL,
                           horizon.price = NULL){
    
  }