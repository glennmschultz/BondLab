  
  # Bond Lab is a software application for the analysis of fixed income
  # securites it provides suite of functions for fixed income analysis
  # with partucluar emphasis on mortgage-backed and asset-backed securities
  # copyright 2016 Bond Lab Technologies, Inc.
  

  #' @include PrepaymentModelTune.R
  NULL
  
  #' An S4 class PrepaymentModelFunctions containing functions
  #' for building an agency prepayment model
  #' @slot TurnoverRate an numeric value the turnover rate
  #' @slot SeasoningRamp a function defining the seasoning ramp
  #' @slot Curtailment a function defining borrower curtailment
  #' @slot SeasonalFactors a function defining the seasonal factors
  #' @slot ArcTanIncentive a three-factor arc tangent function used to define
  #' the borrower response to refinancing incentive
  #' @slot BorrowerBurnout a function defining the borrower burnout rate
  #' @slot DefaultRamp a function defining the default ramp
  #' @slot DefaultOrigLTVMult a function defining the default multipliers
  #' for orig. LTV that are applied to the default ramp
  #' @slot DefaultUpdatedLTVMult a function defining the default multipliers
  #' for change in LTV that are applied to the default ramp
  #' @slot DefaultSATOMult a function defining the default multipliers
  #' for borrower SATO that are applied to the default ramp
  #' @exportClass PrepaymentModelFunctions 
  setClass("PrepaymentModelFunctions",
           representation(
             TurnoverRate = "numeric",
             SeasoningRamp = "function",
             Curtailment = "function",
             SeasonalFactors = "function",
             ArcTanIncentive = "function",
             BorrowerBurnout = "function",
             DefaultRamp = "function",
             DefaultOrigLTVMult = "function",
             DefaultUpdatedLTVMult = "function",
             DefaultSATOMult = "function"
           ))
  
  setGeneric("PrepaymentModelFunctions", function(
             TurnoverRate = "numeric",
             SeasoningRamp = "function",
             Curtailment = "function",
             SeasonalFactors = "function",
             ArcTanIncentive = "function",
             BorrowerBurnout = "function",
             DefaultRamp = "function",
             DefaultOrigLTVMult = "function",
             DefaultUpdatedLTVMult = "function",
             DefaultSATOMult = "function"
             )
  {standardGeneric("PrepaymentModelFunctions")})
  
  # Note: standard generic function TurnoverRate is defined
  # in prepayment model tune
  
  #' A standard generic function to access the slot SeasoningRamp from the class
  #' PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export SeasoningRamp
  setGeneric("SeasoningRamp", function(object)
    {standardGeneric("SeasoningRamp")})
 
  #' A standard generic function to access the slot Curatialment from the class
  #' PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export Curtailment
  setGeneric("Curtailment", function(object)
  {standardGeneric("Curtailment")})
  
  
  #' A standard generic function to access the slot SeasonalFactors from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export SeasonalFactors
  setGeneric("SeasonalFactors", function(object)
    {standardGeneric("SeasonalFactors")})
  
  #' A standard generic function to access the slot ArcTanIncentive from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export ArcTanIncentive
  setGeneric("ArcTanIncentive", function(object)
  {standardGeneric("ArcTanIncentive")})
  
  #' A standard generic function to access the slot BorrowerBurnout from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export BorrowerBurnout
  setGeneric("BorrowerBurnout", function(object)
  {standardGeneric("BorrowerBurnout")})
  
  #' A standard generic function to access the slot DefaultRamp from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export DefaultRamp
  setGeneric("DefaultRamp", function(object)
  {standardGeneric("DefaultRamp")})
 
  #' A standard generic function to access the slot OrigLTVMult from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export DefaultOrigLTVMult
  setGeneric("DefaultOrigLTVMult", function(object)
  {standardGeneric("DefaultOrigLTVMult")})
  
  #' A standard generic function to access the slot UpdatedLTVMult from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export DefaultUpdatedLTVMult
  setGeneric("DefaultUpdatedLTVMult", function(object)
  {standardGeneric("DefaultUpdatedLTVMult")})
  
  #' A standard generic function to access the slot SATOMult from the 
  #' class PrepaymentModelFunctions
  #' @param object an S4 object
  #' @export DefaultSATOMult
  setGeneric("DefaultSATOMult", function(object)
  {standardGeneric("DefaultSATOMult")})
  
  setMethod("initialize",
            signature = ("PrepaymentModelFunctions"),
            function(.Object,
                     TurnoverRate ="numeric",
                     SeasoningRamp = "function",
                     Curtailment = "function",
                     SeasonalFactors = "function",
                     ArcTanIncentive = "function",
                     BorrowerBurnout = "function",
                     DefaultRamp = "function",
                     DefaultOrigLTVMult = "function",
                     DefaultUpdatedLTVMult = "function",
                     DefaultSATOMult = "function")
            {
              callNextMethod(.Object,
                             TurnoverRate = TurnoverRate,
                             SeasoningRamp = SeasoningRamp,
                             Curtailment = Curtailment,
                             SeasonalFactors = SeasonalFactors,
                             ArcTanIncentive = ArcTanIncentive,
                             BorrowerBurnout = BorrowerBurnout,
                             DefaultRamp = DefaultRamp,
                             DefaultOrigLTVMult = DefaultOrigLTVMult,
                             DefaultUpdatedLTVMult = DefaultUpdatedLTVMult,
                             DefaultSATOMult = DefaultSATOMult)
            })
  
  #' A method to extract the slot TurnoverRate from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod TurnoverRate
  setMethod("TurnoverRate", signature("PrepaymentModelFunctions"),
    function(object){object@TurnoverRate})
  
  #' A method to extract the slot SeasoningRamp from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod SeasoningRamp
  setMethod("SeasoningRamp", signature("PrepaymentModelFunctions"),
            function(object){object@SeasoningRamp})
  
  #' A method to extract the slot Curtailment from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod Curtailment
  setMethod("Curtailment", signature("PrepaymentModelFunctions"),
            function(object){object@Curtailment})
  
  #' A method to extract the slot SeasonalFactors from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod SeasonalFactors
  setMethod("SeasonalFactors", signature("PrepaymentModelFunctions"),
            function(object){object@SeasonalFactors})
  
  
  #' A method to extract the slot ArcTanIncentive from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod ArcTanIncentive
  setMethod("ArcTanIncentive", signature("PrepaymentModelFunctions"),
            function(object){object@ArcTanIncentive})
  
  #' A method to extract the slot ArcTanIncentive from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod BorrowerBurnout
  setMethod("BorrowerBurnout", signature("PrepaymentModelFunctions"),
            function(object){object@BorrowerBurnout})
  
  #' A method to extract the slot DefaultRamp from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod DefaultRamp
  setMethod("DefaultRamp", signature("PrepaymentModelFunctions"),
            function(object){object@DefaultRamp})
  
  #' A method to extract the slot OrigLTVMult from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod DefaultOrigLTVMult
  setMethod("DefaultOrigLTVMult", signature("PrepaymentModelFunctions"),
            function(object){object@DefaultOrigLTVMult})
  
  #' A method to extract the slot UpdatedMult from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod DefaultUpdatedLTVMult
  setMethod("DefaultUpdatedLTVMult", signature("PrepaymentModelFunctions"),
            function(object){object@DefaultUpdatedLTVMult})
  
  #' A method to extract the slot UpdatedMult from the object PrepaymentModelFunctions
  #' @param object an S4 object of the class PrepaymentModelFunctions
  #' @exportMethod DefaultSATOMult
  setMethod("DefaultSATOMult", signature("PrepaymentModelFunctions"),
            function(object){object@DefaultSATOMult})
  
  
  #' A constructor function for the class ModelFunctions
  #'  
  #' @param TurnoverRate a numeric value the turnover rate
  #' @param SeasoningRamp a function defining the seasoning ramp.  The input
  #' variables are alpha the aymptote, beta the intercept, theta the point of
  #' maximum curvature.
  #' @param Curtailment a function defining the curtailment
  #' @param SeasonalFactors a function defining the seasonal pattern of prepayments
  #' the input variables are alpha the function's maximum value, Month the numeric
  #' value of the month of the year, theta the point at which the function reaches 
  #' its maximum value
  #' @param ArcTanIncentive a function defining the borrower incentive response
  #' @param BorrowerBurnout a function defining borrower burnout 
  #' @param DefaultRamp a function defining the default ramp
  #' @param DefaultOrigLTVMult a function defining the orig. LTV multipliers
  #' @param DefaultUpdatedLTVMult a function defining the updated LTV default multipliers
  #' @param DefaultSATOMult a function defining the SATO multipliers on the default curve
  PrepaymentModelFunctions <- function(
    TurnoverRate = "numeric",
    SeasoningRamp = "function",
    Curtailment = "function",
    SeasonalFactors = "function",
    ArcTanIncentive = "function",
    BorrowerBurnout = "function",
    DefaultRamp = "function",
    DefaultOrigLTVMult = "function",
    DefaultUpdatedLTVMult = "function",
    DefaultSATOMult = "function"
  )
  {
    new("PrepaymentModelFunctions",
        TurnoverRate = TurnoverRate,
        SeasoningRamp = SeasoningRamp,
        Curtailment = Curtailment,
        SeasonalFactors = SeasonalFactors,
        ArcTanIncentive = ArcTanIncentive,
        BorrowerBurnout = BorrowerBurnout,
        DefaultRamp = DefaultRamp,
        DefaultOrigLTVMult = DefaultOrigLTVMult,
        DefaultUpdatedLTVMult = DefaultUpdatedLTVMult,
        DefaultSATOMult = DefaultSATOMult)
  }
  
  #' A constructor function to create the PrepaymentModelFunctions class
  #' 
  #' There are no inputs model functions are coded into the function constructor
  #' @export MakeModelFunctions
  MakeModelFunctions <- function()
  {
    temp <- PrepaymentModelFunctions(
      TurnoverRate = .08,
      SeasoningRamp = function(alpha = numeric(),
                               beta = numeric(),
                               theta = numeric(),
                               LoanAge = numeric())
      {alpha - beta * exp(-theta * LoanAge)},
      
      Curtailment = function(LoanAge){exp(pmax(0, LoanAge - 330) * .04)},
      
      SeasonalFactors = function(alpha = numeric(), 
                                 Month = numeric(), 
                                 theta = numeric())
        {(1  + alpha *sin((pi/2 * (Month + theta - 3)) / 3 - 1))},
      
      
      ArcTanIncentive = function(incentive = vector(),
                                 theta1 = numeric(),
                                 theta2 = numeric(),
                                 beta = numeric(),
                                 location = numeric())
      {theta1 + theta2 * atan(incentive + pi * (beta * ((location - atan(incentive))/pi)))},
      
      BorrowerBurnout = function(beta1 = numeric(),
                                 beta2 = numeric(),
                                 MaxIncen = numeric(),
                                 LoanAge = numeric())
      {exp(beta1 * LoanAge +  beta2 * MaxIncen)},
      
      DefaultRamp = function(BeginCDR = numeric(),
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
      (DownRamp/DownRampMonths),EndCDR)))},
      
      DefaultOrigLTVMult = function(OrigLTV = numeric(),
                                    MinOLTV = numeric(),
                                    MaxOLTV = numeric(),
                                    MinOrigMultiplier = numeric(),
                                    MaxOrigMultiplier = numeric()){
        ifelse(OrigLTV > MaxOLTV, MaxOrigMultiplier,
        ifelse(OrigLTV > MinOLTV & OrigLTV <= MaxOLTV, 1.0, MinOrigMultiplier))},
      
      DefaultUpdatedLTVMult = function(beta = numeric(), 
                                       OrigLTV = numeric(), 
                                       ULTV = numeric()){
        chgLTV = (OrigLTV - ULTV)/ltv.basis
        exp(-beta * chgLTV)},
      
      DefaultSATOMult = function(beta = numeric(), 
                                 SATO = numeric()) {
        exp(beta * SATO)}
    )
    

    
    SaveModelFunctions(ModelFile = temp)
  }
     
    
  
  
 
  
  
  
  
  