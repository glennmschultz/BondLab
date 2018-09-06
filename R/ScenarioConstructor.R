
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.


  #'@include BondScenario.R MortgageScenario.R TermStructure.R
  NULL

  #' @title an S4 class Horizon Curve 
  #' @family Scenario Analysis
  #' @description The class HorizonCurve holds both the start and horizon curve
  #' the object returned has both start TermStructure object and the horizon 
  #' term structure object
  #' @slot HorizonMos the scenario horizon months
  #' @slot StartCurve the starting curve assumption
  #' @slot HorizonCurve the horizon curve assumption
  #' @slot StartTermStrc the starting TermStructure assumption
  #' @slot HorizonTermStrc the horizon TermStructure assumption
  #' @exportClass ScenarioCurve
  setClass('ScenarioCurve',
           slots = c(HorizonMos = 'numeric',
                     StartCurve = 'list',
                     HorizonCurve = 'list',
                     StartTermStrc = 'TermStructure',
                     HorizonTermStrc = 'TermStructure'))
  
  # Note: Standard Generic HorizonMos found in Mortgage Scenario
  
  #' A standard generic function to access the HorizonCurve slot StartCurve
  #' @param object An S4 class object of the type HorizonCurve
  #' @export StartCurve
  setGeneric('StartCurve', function(object)
    {standardGeneric('StartCurve')})
  
  #' A standard generic function to access the HorizonCurve slot HorizonCurve
  #' @param object An S4 class object of the type HorizonCurve
  #' @export HorizonCurve
  setGeneric('HorizonCurve', function(object)
    {standardGeneric('HorizonCurve')})
  
  #' A standard generic function to access the HorizonCurve slot StartTermStruc
  #' @param object An S4 class object of the type HorizonCurve
  #' @export StartTermStrc
  setGeneric('StartTermStrc', function(object)
    {standardGeneric('StartTermStrc')})
  
  #' A standard generic function to access the HorizonCurve slot HorizonTermStruc
  #' @param object An S4 class object of the HorizonCurve
  #' @export HorizonTermStrc
  setGeneric('HorizonTermStrc', function(object)
    {standardGeneric('HorizonTermStrc')})
  
  #' A standard generic function to access the HorizonCurve slot HorizonTermStruc
  #' @param object An S4 class object of the HorizonCurve
  #' @export ScenarioHorizonMos
  setGeneric('ScenarioHorizonMos', function(object)
  {standardGeneric('ScenarioHorizonMos')})
  
  #' @title HorizonMos method, class ScenarioReturn
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{HorizonMos} over which a scenario covers
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod ScenarioHorizonMos
  setMethod("ScenarioHorizonMos", signature("ScenarioCurve"),
            function(object){object@HorizonMos})
  
  #'@title Start Curve the Scenario Starting Curve
  #'@family Scenario
  #'@description a method to get the \strong{Start Curve} from the HorizonCurve object
  #'@param object The name of the object of type HorizonCurve
  #'@exportMethod StartCurve
  setMethod('StartCurve', signature('ScenarioCurve'),
            function(object){object@StartCurve})
  
  #'@title Horizon Curve the Scenario Horizon Curve
  #'@family Scenario
  #'@description a method to get the \strong{Horizon Curve} from the HorizonCurve object
  #'@param object The name of the object of type Horizon Curve
  #'@exportMethod HorizonCurve
  setMethod('HorizonCurve', signature('ScenarioCurve'),
            function(object){object@HorizonCurve})
  
  #'@title StartTermStruc the Scenario Start Term Structure Assumption
  #'@family Scenario
  #'@description a method to get the \strong{Start Term Structure} from the Horizon Curve object
  #'@param object The name of the object of type Horizon Curve
  #'@exportMethod StartTermStrc
  setMethod('StartTermStrc', signature('ScenarioCurve'),
            function(object){object@StartTermStrc})
  
  #'@title HorizonTermStruc the Scenario Horizon Term Structure Assumption
  #'@family Scenario
  #'@description a method to get the \strong{Horizon Term Structure} from the Horizon Curve object
  #'@param object The name of the object of the type Horizon Curve
  #'@exportMethod HorizonTermStrc
  setMethod('HorizonTermStrc', signature('ScenarioCurve'),
            function(object){object@HorizonTermStrc})
  
  #' @title a constructor function of the class HorizonCurve
  #' @family Scenario Analysis
  #' @description a function to construct the HorizonCurve class.  Currently 
  #' supported scenarios
  #' \itemize{
  #' \item{D300 - parallel down 300 basis points}
  #' \item{D275 - parallel down 275 basis points}
  #' \item{D250 - parallel down 250 basis points}
  #' \item{D225 - parallel down 225 basis points}
  #' \item{D200 - parallel down 200 basis points}
  #' \item{D175 - parallel down 175 basis points}
  #' \item{D150 - parallel down 150 basis points}
  #' \item{D125 - parallel down 125 basis points}
  #' \item{D100 - parallel down 100 basis points}
  #' \item{D75 - parallel down 75 basis points}
  #' \item{D50 - parallel down 50 basis points}
  #' \item{D25 - parallel down 25 basis points}
  #' \item{NC - No Change}
  #' \item{U25 - parallel up 25 basis points}
  #' \item{U50 - parallel up 50 basis points}
  #' \item{U75 - parallel up 75 basis points}
  #' \item{U100 - parallel up 100 basis points}
  #' \item{U125 - parallel up 125 basis points}
  #' \item{U150 - parallel up 150 basis points}
  #' \item{U175 - parallel up 175 basis points}
  #' \item{U200 - parallel up 200 basis points}
  #' \item{U225 - parallel up 225 basis points}
  #' \item{U250 - parallel up 250 basis points}
  #' \item{U275 - parallel up 275 basis points}
  #' \item{U300 - parallel up 300 basis points}
  #' } 
  #' @param rates.data A character string referencing a rates object
  #' @param settlement.date A character string the settlement date 'mm-dd-YYYY'
  #' @param horizon.months A numeric value the horizon in months
  #' @param scenario A character string the scenario
  #' @param method A character string indicating the fitting method ns = Nelson
  #' Siegel, dl = Diebond Lee, sv = Severson, asv = adjusted Severson, 
  #' cs = cubic spline (not yet implemented).  For additional details see the termstrc
  #' documentation. 
  #' @export HorizonCurveShift
  HorizonCurveShift <- function(rates.data,
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
  
  new("ScenarioCurve",
      HorizonMos = horizon.months,
      StartCurve = start,
      HorizonCurve = horizon,
      StartTermStrc = start.termstructure,
      HorizonTermStrc = horizon.termstructure)}
  
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
