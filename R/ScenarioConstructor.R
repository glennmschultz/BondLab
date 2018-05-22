
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
  
  # The following script analyzes a pass-through security.  
  # To create the script the standard procedure is followed set class, 
  # set generics, set methods, functions the generics and methods are getters 
  # for the class MortgageCashFlow and the initialize method for the class.  
  # This class is a subclass of the following: (document the superclasses)
  # for the most part this script is requiring only modest changes.

  #' @title an S4 class Horizon Curve 
  #' @family Scenario Analysis
  #' @description The class HorizonCurve holds both the start and horizon curve
  #' the object returned has both start TermStructure object and the horizon 
  #' term structure object
  #' @slot Start the starting TermStructure assumption
  #' @slot Horizon the horizon TermStructure assumption
  #' @exportClass HorizonCurve
  setClass('HorizonCurve',
           slots = c(Start = 'TermStructure',
                     Horizon = 'TermStructure'))
  
  #' @title a constructor function of the class HorizonCurve
  #' @family Scenario Analysis
  #' @description a function to construct the HorizonCurve class 
  #' @param rates.data A character string referencing a rates object
  #' @param settlement.date A character string the settlement date 'mm-dd-YYYY'
  #' @param horizon.months A numeric value the horizon in months
  #' @param scenario A character string the scenario
  #' @param method A character string indicating the fitting method ns = Nelson
  #' Siegel, dl = Diebond Lee, sv = Severson, asv = adjusted Severson, 
  #' cs = cubic spline (not yet implemented).  For additional details see the termstrc
  #' documentation. 
  #' @export HorizonCurve
  HorizonCurve <- function(rates.data,
                           settlement.date,
                           horizon.months = 12,
                           scenario = 'NC',
                           method = 'dl'){
  horizon.settlement <- as.Date(settlement.date, format = "%m-%d-%Y") %m+% months(horizon.months)  
  start <- rates.data
  horizon <- rates.data
  # Calculate the horizon curve based on the scenario
  Scenario <- ScenarioCall(Scenario = scenario)
  horizon[1,2:length(horizon)] <- as.numeric(ScenarioFormula(Scenario)(start[1,2:length(start)], 
                                                                       Shiftbps = Shiftbps(Scenario)))
  horizon[1,1] <- as.character(as.Date(horizon[1,1]) %m+% months(horizon.months))
  # Calculate the term structure objects
  invisible(capture.output(start.termstructure <- TermStructure(rates.data = start, method = method)))
  invisible(capture.output(horizon.termstructure <- TermStructure(rates.data = horizon, method = method)))
  
  new("HorizonCurve",
      Start = start.termstructure,
      Horizon = horizon.termstructure)}
  
  #' @title An S4 class Scenario
  #' @family Scenario Analysis
  #' @description 
  #' The interest rate shift function applied the yield curve.  The shift may
  #' be applied to the spot rate curve slot of the object TermStructure or the
  #' rates data object.  The BondLab convention is to identify shifts applied to
  #' the sopt rate curve with a trailing for example D50s is down 50bps applied 
  #' to the spot rate curve.  D50 is down 50bps applied to the rates data object.
  #' In all cases the curve shift is not allowed to result in a negative rate. 
  #' Thus, all shifts are subject to a floor of 0.01.  It is recommend that those
  #' users wishing to create custom yield shift scenarios avoid 0 or negative 
  #' rates when creating custom shift scenarios.
  #' @slot Name A character the name of the scenario
  #' @slot Type A character the type of scenario eg("Immediate", "Gradual")
  #' @slot ShiftType A character indicating the type shift 
  #' eg("Parallel", "Twist") 
  #' @slot Shiftbps A numeric value the interest rate shift in bps
  #' @slot Formula A function defining the interest rate shift
  #' @exportClass Scenario
  setClass("Scenario",
         representation(
           Name = "character",
           Type = "character",
           ShiftType = "character",
           Shiftbps = "character",
           Formula = "function"
         ))

  #' A standard generic to access the slot Name
  #' @param object An S4 class object of the type Scenario
  #' @export Name
  setGeneric("Name", function(object)
  {standardGeneric("Name")})
  
  #' A standard generic to access the slot Type
  #' @param object An S4 class object of the type Scenario
  #' @export Type
  setGeneric("Type", function(object)
  {standardGeneric("Type")})
  
  #' A standard generic to access the slot ShiftType
  #' @param object An S4 class object of the type Scenario
  #' @export ShiftType
  setGeneric("ShiftType", function(object)
  {standardGeneric("ShiftType")})
  
  #' A standard generic to access the slot Shiftbps
  #' @param object An S4 class object of the type Scenario
  #' @export Shiftbps
  setGeneric("Shiftbps", function(object)
  {standardGeneric("Shiftbps")})
  
  #' A standard generic to access the slot Formula
  #' @param object An S4 class object of the type Scenario
  #' @export ScenarioFormula
  setGeneric("ScenarioFormula", function(object)
  {standardGeneric("ScenarioFormula")})

  setMethod("initialize",
          signature("Scenario"),
          function(.Object,
                   Name = "character",
                   Type = "character",
                   ShiftType = "character",
                   Shiftbps = "character",
                   Formula = "function",
                   ...){
            callNextMethod(.Object,
                          Name = Name,
                          Type = Type,
                          ShiftType = ShiftType,
                          Shiftbps = Shiftbps,
                          Formula = Formula,
                          ...)
                          })
  
  #' @title Scenario Name
  #' @family Scenario Analysis
  #' @description A method to get \strong{Name} from the object scenario.  This
  #' can be useful for those investors saving the results of multiple scenario
  #' analysis.
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod Name
  setMethod("Name", signature("Scenario"),
            function(object){object@Name})
  
  #' A Method to extract scenario type from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod Type
  setMethod("Type", signature("Scenario"),
            function(object){object@Type})

  #' A Method to extract ShiftType from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod ShiftType
  setMethod("ShiftType", signature("Scenario"),
            function(object){object@ShiftType})
  
  #' A Method to extract Shiftbps from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod Shiftbps
  setMethod("Shiftbps", signature("Scenario"),
            function(object){object@Shiftbps})
  
  #' A Method to extract the scenario formula from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod ScenarioFormula
  setMethod("ScenarioFormula", signature("Scenario"),
            function(object){object@Formula})

  RateScenario <- function(
    Name = "character",
    Type = "character",
    ShiftType = "character",
    Shiftbps = "character",
    Formula = "function")
  {
    new("Scenario",
        Name = Name,
        Type = Type,
        ShiftType = ShiftType,
        Shiftbps = Shiftbps,
        Formula = Formula)}

  #' A constructor function of the class Scenario
  #' 
  #' This is a standard generic function used to construct the class Scenario.
  #' The Scenario class is applied to the either a Rates object or a 
  #' TermStructure object
  #' 
  #' @param Name A character string indicating the unique name which 
  #' identifies the scenario
  #' @param Type A character string indicating the type of scenario 
  #' "aggressive", "moderate"
  #' @param ShiftType A character string indicating the type interest rate 
  #' shift "Parallel", "Twist", "etc"
  #' @param Shiftbps A numeric vector indicating the shift applied to each 
  #' point on the curve
  #' @param Formula A function to apply the shift to each pont on the curve
  #' @examples
  #' \dontrun{
  #' MakeScenario(Name = "Flat50",
  #' Type = "Immediate",
  #' ShiftType = "Twist",
  #' Shiftbps = -75,
  #' Formula = function(rates.data, Shiftbps){
  #' as.character(as.numeric(rates.data[1,2:length(rates.data)]) + 
  #' Shiftbps/yield.basis)})}  
  #' @export Scenario
  Scenario <- function(
  Name = "character",
  Type = "character",
  ShiftType = "character",
  Shiftbps = "character",
  Formula = "function")
  {
  temp <- RateScenario(
    Name = Name,
    Type = Type,
    ShiftType = ShiftType,
    Shiftbps = Shiftbps,
    Formula = Formula)
  
  SaveScenario(Scenario = Name, ScenarioFile = temp)
  }
