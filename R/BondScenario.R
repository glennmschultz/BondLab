
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
  
  #' @include ScenarioConstructor.R BondCashFlow.R TermStructure.R
  #' @include BondKeyRate.R MortgageScenario.R CurveSpreads.R
  NULL
  
  #' An S4 class a list of bond scenarios for analysis
  #' 
  #' The class BondScenarioSet is a list of classes of the type scenario
  #' @slot Scenario A list of the classes of the type scenario
  #' @exportClass BondScenarioSet
  setClass("BondScenarioSet",
           representation(
             Scenario = "list"
           ))
  
  setGeneric("BondScenarioSet", function(object)
             {standardGeneric("BondScenarioSet")})
  
  # Note: standardGeneric ScenarioSet is found in MortgageScenario.R
  
  setMethod("initialize",
            signature("BondScenarioSet"),
            function(.Object,
                     Scenario = "list",
                     ...)
            {
              callNextMethod(.Object,
                            Scenario = Scenario,
                            ...)
            })

  #' A Method to extract the slot Scenario from the object BondScenarioSet
  #' 
  #' @param object The name of an S4 object of the type BondScenarioSet
  #' @exportMethod ScenarioSet
  setMethod("ScenarioSet", signature("BondScenarioSet"),
            function(object){object@Scenario})