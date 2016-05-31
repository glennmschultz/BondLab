  
  # Bond Lab is a software application for the analysis of fixed income
  # securites it provides suite of functions for fixed income analysis
  # with partucluar emphasis on mortgage-backed and asset-backed securities
  # copyright 2016 Bond Lab Technologies, Inc.
  

  #' @include PrepaymentModelTune.R
  NULL
  
  #' An S4 class PrepaymentModelFunctions containing functions
  #' for building an agency prepayment model
  #' @slot TurnoverRate an numeric value the Turnover Rate
  #' @slot SeasoningRamp a function defining the seasoning ramp
  #' @exportClass PrepaymentModelFunctions 
  setClass("PrepaymentModelFunctions",
           representation(
             TurnoverRate = "numeric",
             SeasoningRamp = "function"
           ))
  
  # Note: standard generic function TurnoverRate is defined
  # in prepayment model tune
  
  #' A standard generic function to access the slot Seasoning from the class
  #' PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export SeasoningRamp
  setGeneric("SeasoningRamp", function(object)
    {standardGeneric("SeasoningRamp")})
  
  #' A method to extract the slot TurnoverRate from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod TurnoverRate
  setMethod("TurnoverRate", signature("PrepaymentModelFunctions"),
    function(object){object@TurnoverRate})
  
  #' A method to extract the slot Seasoning from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod SeasoningRamp
  setMethod("SeasoningRamp", signature("PrepaymentModelFunctions"),
            function(object){object@Seasoning})
  