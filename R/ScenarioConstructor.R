
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.
  
  # The following script analyzes a pass-through security.  
  # To create the script the standard procedure is followed set class, 
  # set generics, set methods, functions the generics and methods are getters 
  # for the class MortgageCashFlow and the initialize method for the class.  
  # This class is a subclass of the following: (document the superclasses)
  # for the most part this script is requiring only modest changes.
  
  #' An S4 class Scenario to describing the interest rate scenario
  #' and interest rate shift function applied the curve
  #' @slot Name A character the name of the scenario
  #' @slot Type A character the type of scenario eg("Aggressive", "Gradual",
  #' "Immediate")
  #' @slot Horizon A character indicating the time horizon over which the 
  #' scenario evolves
  #' @slot ShiftType A character indicating the type shift 
  #' eg("Parallel", "Twist") 
  #' @slot Shiftbps A numeric value the interest rate shift in bps
  #' @slot Formula A function defining the interest rate shift
  #' @exportClass Scenario
  setClass("Scenario",
         representation(
           Name = "character",
           Type = "character",
           Horizon = "character",
           ShiftType = "character",
           Shiftbps = "character",
           Formula = "function"
         ))
  
  setGeneric("Scenario", function(Name = "character",
                                      Type = "character",
                                      Horizon = "character",
                                      ShiftType = "character",
                                      Shiftbps = "character",
                                      Formula = "function")
  {standardGeneric("Scenario")})
  
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
  
  #' A standard generic to access the slot Horizon
  #' @param object An S4 class object of the type Scenario
  #' @export Horizon
  setGeneric("Horizon", function(object)
  {standardGeneric("Horizon")})
  
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
                   Horizon = "character",
                   ShiftType = "character",
                   Shiftbps = "character",
                   Formula = "function",
                   ...){
            callNextMethod(.Object,
                          Name = Name,
                          Type = Type,
                          Horizon = Horizon,
                          ShiftType = ShiftType,
                          Shiftbps = Shiftbps,
                          Formula = Formula,
                          ...)
                          })
  
  #' A Method to extract scenario name from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod Name
  setMethod("Name", signature("Scenario"),
            function(object){object@Name})
  
  #' A Method to extract scenario type from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod Type
  setMethod("Type", signature("Scenario"),
            function(object){object@Type})
  
  #' A Method to extract scenario Horizon from an S4 class Scenario
  #' @param object The name of the object of the S4 class of type Scenario
  #' @exportMethod Horizon
  setMethod("Horizon", signature("Scenario"),
            function(object){object@Horizon})
  
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
    Horizon = "character",
    ShiftType = "character",
    Shiftbps = "character",
    Formula = "function")
  {
    new("Scenario",
        Name = Name,
        Type = Type,
        Horizon = Horizon,
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
  #' @param Horizon A character string indicating the time horizon over 
  #' which the scenario evolves
  #' @param ShiftType A character string indicating the type interest rate 
  #' shift "Parallel", "Twist", "etc"
  #' @param Shiftbps A numeric vector indicating the shift applied to each 
  #' point on the curve
  #' @param Formula A function to apply the shift to each pont on the curve
  #' @examples
  #' \dontrun{
  #' MakeScenario(Name = "Flat50",
  #' Type = "Agrressive",
  #' Horizon = "Immediate",
  #' ShiftType = "Twist",
  #' Shiftbps = c(rep(25,7),0, rep(-25,3)),
  #' Formula = function(rates.data, Shiftbps){
  #' as.character(as.numeric(rates.data[1,2:length(rates.data)]) + 
  #' Shiftbps/yield.basis)})}  
  #' @export Scenario
  Scenario <- function(
  Name = "character",
  Type = "character",
  Horizon = "character",
  ShiftType = "character",
  Shiftbps = "character",
  Formula = "function")
  {
  temp <- RateScenario(
    Name = Name,
    Type = Type,
    Horizon = Horizon,
    ShiftType = ShiftType,
    Shiftbps = Shiftbps,
    Formula = Formula)
  
  SaveScenario(Scenario = Name, ScenarioFile = temp)
  }
