  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # File License
  # Copyright (C) 2015  Bond Lab Technologies, Inc.

  setClass("PrepaymentModelTune",
         representation(
           TurnoverRate = "numeric",
           Turnover.alpha = "numeric",
           Turnover.beta = "numeric",
           Turnover.theta = "numeric",
           Seasonality.alpha = "numeric",
           Seasonality.theta = "numeric",
           Incentive.Fast.theta.1 = "numeric",
           Incentive.Fast.theta.2 = "numeric",
           Incentive.Fast.beta = "numeric",
           Incentive.Fast.eta = "numeric",
           Incentive.Slow.theta.1 = "numeric",
           Incentive.Slow.theta.2 = "numeric",
           Incentive.Slow.beta = "numeric",
           Incentive.Slow.eta = "numeric",
           Burnout.beta.1 = "numeric",
           Burnout.beta.2 = "numeric",
           BeginCDR = "numeric",
           PeakCDR = "numeric",
           EndCDR = "numeric",
           PeakMonth = "numeric",
           PlateauMonths = "numeric",
           EndMonth = "numeric",
           MinOrigLTV = "numeric",
           MaxOrigLTV = "numeric",
           MinOrigMultiplier = "numeric",
           MaxOrigMultiplier = "numeric",
           UpdatedLTV.beta = "numeric",
           SATO.beta = "numeric"
         ))

  setGeneric("MakeModelTune", function(
    ModelName = "character",
    TurnoverRate = "numeric",
    Turnover.alpha = "numeric",
    Turnover.beta = "numeric",
    Turnover.theta = "numeric",
    Seasonality.alpha = "numeric",
    Seasonality.theta = "numeric",
    Incentive.Fast.theta.1 = "numeric",
    Incentive.Fast.theta.2 = "numeric",
    Incentive.Fast.beta = "numeric",
    Incentive.Fast.eta = "numeric",
    Incentive.Slow.theta.1 = "numeric",
    Incentive.Slow.theta.2 = "numeric",
    Incentive.Slow.beta = "numeric",
    Incentive.Slow.eta = "numeric",
    Burnout.beta.1 = "numeric",
    Burnout.beta.2 = "numeric",
    BeginCDR = "numeric",
    PeakCDR = "numeric",
    EndCDR = "numeric",
    PeakMonth = "numeric",
    PlateauMonths = "numeric",
    EndMonth = "numeric",
    MinOrigLTV = "numeric",
    MaxOrigLTV = "numeric",
    MinOrigMultiplier = "numeric",
    MaxOrigMultiplier = "numeric",
    UpdatedLTV.beta = "numeric",
    SATO.beta = "numeric")
  
  {standardGeneric("MakeModelTune")})
  
  #' A standard generic function to access the slot TuroverRate
  #' @param object an S4 object
  #' @export TurnoverRate
  setGeneric("TurnoverRate", function(object)
    {standardGeneric("TurnoverRate")})
  
  #' A standard generic function to access the slot Turnover.alpha
  #' @param object an S4 object
  #' @export TurnoverAlpha
  setGeneric("TurnoverAlpha", function(object)
    {standardGeneric("TurnoverAlpha")})
  
  #' A standard generic function to access the slot Turnover.beta
  #' @param object an S4 object
  #' @export TurnoverBeta
  setGeneric("TurnoverBeta", function(object)
  {standardGeneric("TurnoverBeta")})
  
  #' A standard generic function to access the slot Turnover.theta
  #' @param object an S4 object
  #' @export TurnoverTheta
  setGeneric("TurnoverTheta", function(object)
  {standardGeneric("TurnoverTheta")})
  
  #' A standard generic function to access the slot Seasonality.alpha
  #' @param object an S4 object
  #' @export SeasonalityAlpha
  setGeneric("SeasonalityAlpha", function(object)
  {standardGeneric("SeasonalityAlpha")})
  
  #' A standard generic function to access the slot Seasonality.theta
  #' @param object an S4 object
  #' @export SeasonalityTheta
  setGeneric("SeasonalityTheta", function(object)
  {standardGeneric("SeasonalityTheta")})
  
  #' A standard generic function to access the slot Incentive.Fast.theta.1
  #' @param object an S4 object
  #' @export IncentiveFastThetaOne
  setGeneric("IncentiveFastThetaOne", function(object)
  {standardGeneric("IncentiveFastThetaOne")})
  
  #' A standard generic function to access the slot Incentive.Fast.theta.2
  #' @param object an S4 object
  #' @export IncentiveFastThetaTwo
  setGeneric("IncentiveFastThetaTwo", function(object)
  {standardGeneric("IncentiveFastThetaTwo")})
  
  #' A standard generic function to access the slot Incentive.Fast.beta
  #' @param object an S4 object
  #' @export IncentiveFastBeta
  setGeneric("IncentiveFastBeta", function(object)
  {standardGeneric("IncentiveFastBeta")})
  
  #' A standard generic function to access the slot Incentive.Fast.eta
  #' @param object an S4 object
  #' @export IncentiveFastEta
  setGeneric("IncentiveFastEta", function(object)
  {standardGeneric("IncentiveFastEta")})
  
  #' A standard generic function to access the slot Incentive.Slow.theta.1
  #' @param object an S4 object
  #' @export IncentiveSlowThetaOne
  setGeneric("IncentiveSlowThetaOne", function(object)
  {standardGeneric("IncentiveSlowThetaOne")})
  
  #' A standard generic function to access the slot Incentive.Slow.theta.2
  #' @param object an S4 object
  #' @export IncentiveSlowThetaTwo
  setGeneric("IncentiveSlowThetaTwo", function(object)
  {standardGeneric("IncentiveSlowThetaTwo")})
  
  #' A standard generic function to access the slot Incentive.Slow.beta
  #' @param object an S4 object
  #' @export IncentiveSlowBeta
  setGeneric("IncentiveSlowBeta", function(object)
    {standardGeneric("IncentiveSlowBeta")})
  
  #' A standard generic function to access the slot Incentive.Slow.beta
  #' @param object an S4 object
  #' @export IncentiveSlowEta
  setGeneric("IncentiveSlowEta", function(object)
  {standardGeneric("IncentiveSlowEta")})
  
  #' A standard generic functin to access the slot Burnout.beta.1
  #' @param object an S4 object
  #' @export BurnoutBetaOne
  setGeneric("BurnoutBetaOne", function(object)
    {standardGeneric("BurnoutBetaOne")})
  
  #' A standard generic function to access the slot Burnout.beta.e
  #' @param object an S4 object
  #' @export BurnoutBetaTwo
  setGeneric("BurnoutBetaTwo", function(object)
    {standardGeneric("BurnoutBetaTwo")})
  
  #' A standard generic function to access the slot BeginCDR
  #' @param object an S4 object
  #' @export BeginCDR
  setGeneric("BeginCDR", function(object)
    {standardGeneric("BeginCDR")})
  
  #' A standard generic functin to access the slot PeakCDR
  #' @param object an S4 object
  #' @export PeakCDR
  setGeneric("PeakCDR", function(object)
    {standardGeneric("PeakCDR")})
  
  #' A standard generic function to access the slot EndCDR
  #' @param object an S4 object
  #' @export EndCDR
  setGeneric("EndCDR", function(object)
    {standardGeneric("EndCDR")})
  
  #' A standard generic function to access the slot PeakMonth
  #' @param object an S4 object
  #' @export PeakMonth
  setGeneric("PeakMonth", function(object)
    {standardGeneric("PeakMonth")})
  
  #' A standard generic function to access the slot PlateauMonths
  #' @param object an S4 object
  #' @export PlateauMonths
  setGeneric("PlateauMonths", function(object)
    {standardGeneric("PlateauMonths")})
  
  #' A standard generic function to access the slot EndMonth
  #' @param object an S4 object
  #' @export EndMonth
  setGeneric("EndMonth", function(object)
    {standardGeneric("EndMonth")})
  
  #' A standard generic function to access the slot MinOrigLTV
  #' @param object an S4 object
  #' @export MinOrigLTV
  setGeneric("MinOrigLTV", function(object)
    {standardGeneric("MinOrigLTV")})
  
  #' A standard generic functin to access the slot MaxOrigLTV
  #' @param object an S4 object
  #' @export MaxOrigLTV
  setGeneric("MaxOrigLTV", function(object)
    {standardGeneric("MaxOrigLTV")})
  
  #' A standard generic function to access the slot MinOrigMultiplier
  #' @param object an S4 object
  #' @export MinOrigMultiplier
  setGeneric("MinOrigMultiplier", function(object)
    {standardGeneric("MinOrigMultiplier")})
  
  #' A standard generic function to access the slot MaxOrigMultiplier
  #' @param object an S4 object
  #' @export MaxOrigMultiplier
  setGeneric("MaxOrigMultiplier", function(object)
    {standardGeneric("MaxOrigMultiplier")})
  
  #' A standard generic function to access the slot UpdatedLTV.beta
  #' @param object an S4 object
  #' @export UpdatedLTVBeta
  setGeneric("UpdatedLTVBeta", function(object)
  {standardGeneric("UpdatedLTVBeta")})
  
  #' A standard generic function to access the slot SATO.beta
  #' @param object an S4 object
  #' @export SATOBeta
  setGeneric("SATOBeta", function(object)
  {standardGeneric("SATOBeta")})
 
  setMethod("initialize",
          signature("PrepaymentModelTune"),
          function(.Object,
                   TurnoverRate = "numeric",
                   Turnover.alpha = "numeric",
                   Turnover.beta = "numeric",
                   Turnover.theta = "numeric",
                   Seasonality.alpha = "numeric",
                   Seasonality.theta = "numeric",
                   Incentive.Fast.theta.1 = "numeric",
                   Incentive.Fast.theta.2 = "numeric",
                   Incentive.Fast.beta = "numeric",
                   Incentive.Fast.eta = "numeric",
                   Incentive.Slow.theta.1 = "numeric",
                   Incentive.Slow.theta.2 = "numeric",
                   Incentive.Slow.beta = "numeric",
                   Incentive.Slow.eta = "numeric",
                   Burnout.beta.1 = "numeric",
                   Burnout.beta.2 = "numeric",
                   BeginCDR = "numeric",
                   PeakCDR = "numeric",
                   EndCDR = "numeric",
                   PeakMonth = "numeric",
                   PlateauMonths = "numeric",
                   EndMonth = "numeric",
                   MinOrigLTV = "numeric",
                   MaxOrigLTV = "numeric",
                   MinOrigMultiplier = "numeric",
                   MaxOrigMultiplier = "numeric",
                   UpdatedLTV.beta = "numeric",
                   SATO.beta = "numeric")
          {
            callNextMethod(.Object,
            TurnoverRate = TurnoverRate,
            Turnover.alpha = Turnover.alpha,
            Turnover.beta = Turnover.beta,
            Turnover.theta = Turnover.theta,
            Seasonality.alpha = Seasonality.alpha,
            Seasonality.theta = Seasonality.theta,
            Incentive.Fast.theta.1 = Incentive.Fast.theta.1,
            Incentive.Fast.theta.2 = Incentive.Fast.theta.2,
            Incentive.Fast.beta = Incentive.Fast.beta,
            Incentive.Fast.eta = Incentive.Fast.eta,
            Incentive.Slow.theta.1 = Incentive.Slow.theta.1,
            Incentive.Slow.theta.2 = Incentive.Slow.theta.2,
            Incentive.Slow.beta = Incentive.Slow.beta,
            Incentive.Slow.eta = Incentive.Slow.eta,
            Burnout.beta.1 = Burnout.beta.1,
            Burnout.beta.2 = Burnout.beta.2,
            BeginCDR = BeginCDR,
            PeakCDR = PeakCDR,
            EndCDR = EndCDR,
            PeakMonth = PeakMonth,
            PlateauMonths = PlateauMonths,
            EndMonth = EndMonth,
            MinOrigLTV = MinOrigLTV,
            MaxOrigLTV = MaxOrigLTV,
            MinOrigMultiplier = MinOrigMultiplier,
            MaxOrigMultiplier = MaxOrigMultiplier,
            UpdatedLTV.beta = UpdatedLTV.beta,
            SATO.beta = SATO.beta)
          })
  #' A method to extract TurnoverRate from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod TurnoverRate
  setMethod("TurnoverRate", signature("PrepaymentModelTune"),
            function(object){object@TurnoverRate})
  
  #' A method to extract Turnover.alpha from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod TurnoverAlpha
  setMethod("TurnoverAlpha", signature("PrepaymentModelTune"),
            function(object){object@Turnover.alpha})
  
  #' A method to extract Turnover.beta from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod TurnoverBeta
  setMethod("TurnoverBeta", signature("PrepaymentModelTune"),
            function(object){object@Turnover.beta})
  
  #' A method to extract Turnover.theta from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod TurnoverTheta
  setMethod("TurnoverTheta", signature("PrepaymentModelTune"),
            function(object){object@Turnover.theta})
  
  #' A method to extract Seasonality.alpha from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod SeasonalityAlpha
  setMethod("SeasonalityAlpha", signature("PrepaymentModelTune"),
            function(object){object@Seasonality.alpha})
  
  #' A method to extract Seasonality.theta from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod SeasonalityTheta
  setMethod("SeasonalityTheta", signature("PrepaymentModelTune"),
            function(object){object@Seasonality.theta})
  
  #' A method to extract Incentive.Fast.theta.1 from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveFastThetaOne
  setMethod("IncentiveFastThetaOne", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Fast.theta.1})
  
  #' A method to extract Incentive.Fast.theta.2 from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveFastThetaTwo
  setMethod("IncentiveFastThetaTwo", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Fast.theta.2})
  
  #' A method to extract Incentive.Fast.beta from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveFastBeta
  setMethod("IncentiveFastBeta", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Fast.beta})
  
  #' A method to extract Incentive.Slow.theta.1 from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveSlowThetaOne
  setMethod("IncentiveSlowThetaOne", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Slow.theta.1})
  
  #' A method to extract Incentive.Slow.theta.2 from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveSlowThetaTwo
  setMethod("IncentiveSlowThetaTwo", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Slow.theta.2})
  
  #' A method to extract Incentive.Slow.beta from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveSlowBeta
  setMethod("IncentiveSlowBeta", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Slow.beta})
  
  #' A method to extract Incentive.Slow.eta from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod IncentiveSlowEta
  setMethod("IncentiveSlowEta", signature("PrepaymentModelTune"),
            function(object){object@Incentive.Slow.eta})
  
  #' A method to extract Burnout.beta.1 from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod BurnoutBetaOne
  setMethod("BurnoutBetaOne", signature("PrepaymentModelTune"),
            function(object){object@Burnout.beta.1})
  
  #' A method to extract Burnout.beta.2 from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod BurnoutBetaTwo
  setMethod("BurnoutBetaTwo", signature("PrepaymentModelTune"),
            function(object){object@Burnout.beta.2})
  
  #' A method to extract BeginCDR from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod BeginCDR
  setMethod("BeginCDR", signature("PrepaymentModelTune"),
            function(object){object@BeginCDR})
  
  #' A method to extract PeakCDR from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod PeakCDR
  setMethod("PeakCDR", signature("PrepaymentModelTune"),
            function(object){object@PeakCDR})
  
  #' A method to extract EndCDR from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod EndCDR
  setMethod("EndCDR", signature("PrepaymentModelTune"),
            function(object){object@EndCDR})
  
  #' A method to extract PeakMonth from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod PeakMonth
  setMethod("PeakMonth", signature("PrepaymentModelTune"),
            function(object){object@PeakMonth})
  
  #' A method to extract PlateauMonths from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod PlateauMonths
  setMethod("PlateauMonths", signature("PrepaymentModelTune"),
            function(object){object@PlateauMonths})
  
  #' A method to extract EndMonth from class PrepaymentModelTune
  #' @param object The name of the object of class PrepaymentModelTune
  #' @exportMethod EndMonth
  setMethod("EndMonth", signature("PrepaymentModelTune"),
            function(object){object@EndMonth})
  
  #' A method to extract the MinOrigLTV from class PrepaymentModelTune
  #' @param object the name of the object of class PrepaymentModelTune
  #' @exportMethod MinOrigLTV
  setMethod("MinOrigLTV", signature("PrepaymentModelTune"),
            function(object){object@MinOrigLTV})
  
  #' A method to extract the MaxOrigLTV from class PrepaymentModelTune
  #' @param object the name of the object of class PrepaymentModelTune
  #' @exportMethod MaxOrigLTV
  setMethod("MaxOrigLTV", signature("PrepaymentModelTune"),
            function(object){object@MinOrigLTV})
  
  
  #' A method to extract the MinOrigMultiplier from class PrepaymentModelTune
  #' @param object the name of the object of class PrepaymentModelTune
  #' @exportMethod MinOrigMultiplier
  setMethod("MinOrigMultiplier", signature("PrepaymentModelTune"),
            function(object){object@MinOrigMultiplier})
  
  #' A method to extract the MaxOrigMultiplier from class PrepaymentModelTune
  #' @param object the name of the object of class PrepaymentModelTune
  #' @exportMethod MaxOrigMultiplier
  setMethod("MaxOrigMultiplier", signature("PrepaymentModelTune"),
            function(object){object@MaxOrigMultiplier})
  
  #' A method to extract the UpdatedLTV.beta from class PrepaymentModelTune
  #' @param object the name of the object of class PrepaymentModelTune
  #' @exportMethod UpdatedLTVBeta
  setMethod("UpdatedLTVBeta", signature("PrepaymentModelTune"),
            function(object){object@UpdatedLTV.beta})
  
  #' A method to extract the SATO.beta from class PrepaymentModelTune
  #' @param object the name of the object of class PrepaymentModelTune
  #' @exportMethod SATOBeta
  setMethod("SATOBeta", signature("PrepaymentModelTune"),
            function(object){object@SATOBeta})
  
        ModelTuningParam <- function(
        TurnoverRate = "numeric",
        Turnover.alpha = "numeric",
        Turnover.beta = "numeric",
        Turnover.theta = "numeric",
        Seasonality.alpha = "numeric",
        Seasonality.theta = "numeric",
        Incentive.Fast.theta.1 = "numeric",
        Incentive.Fast.theta.2 = "numeric",
        Incentive.Fast.beta = "numeric",
        Incentive.Fast.eta = "numeric",
        Incentive.Slow.theta.1 = "numeric",
        Incentive.Slow.theta.2 = "numeric",
        Incentive.Slow.beta = "numeric",
        Incentive.Slow.eta = "numeric",
        Burnout.beta.1 = "numeric",
        Burnout.beta.2 = "numeric",
        BeginCDR = "numeric",
        PeakCDR = "numeric",
        EndCDR = "numeric",
        PeakMonth = "numeric",
        PlateauMonths = "numeric",
        EndMonth = "numeric",
        MinOrigLTV = "numeric",
        MaxOrigLTV = "numeric",
        MinOrigMultiplier = "numeric",
        MaxOrigMultiplier = "numeric",
        UpdatedLTV.beta = "numeric",
        SATO.beta = "numeric"
        ){
          new("PrepaymentModelTune",
          TurnoverRate = TurnoverRate,
          Turnover.alpha = Turnover.alpha,
          Turnover.beta = Turnover.beta,
          Turnover.theta = Turnover.theta,
          Seasonality.alpha = Seasonality.alpha,
          Seasonality.theta = Seasonality.theta,
          Incentive.Fast.theta.1 = Incentive.Fast.theta.1,
          Incentive.Fast.theta.2 = Incentive.Fast.theta.2,
          Incentive.Fast.beta = Incentive.Fast.beta,
          Incentive.Fast.eta = Incentive.Fast.eta,
          Incentive.Slow.theta.1 = Incentive.Slow.theta.1,
          Incentive.Slow.theta.2 = Incentive.Slow.theta.2,
          Incentive.Slow.beta = Incentive.Slow.beta,
          Incentive.Slow.eta = Incentive.Slow.eta,
          Burnout.beta.1 = Burnout.beta.1,
          Burnout.beta.2 = Burnout.beta.2,
          BeginCDR = BeginCDR,
          PeakCDR = PeakCDR,
          EndCDR = EndCDR,
          PeakMonth = PeakMonth,
          PlateauMonths = PlateauMonths,
          EndMonth = EndMonth,
          MinOrigLTV = MinOrigLTV,
          MaxOrigLTV = MaxOrigLTV,
          MinOrigMultiplier = MinOrigMultiplier,
          MaxOrigMultiplier = MaxOrigMultiplier,
          UpdatedLTV.beta = UpdatedLTV.beta,
          SATO.beta = SATO.beta)
        }
    
    #' A constructor function to create the prepayment model tuning parameters
    #' 
    #' Create custom prepayment model tuning parameters that may
    #' be applied to specific collateral cohorts
    #' @param ModelName a character string.  The name of the model.
    #' @param TurnoverRate a numeric value.  The estimated housing turnover rate.
    #' @param Turnover.alpha a numeric value. The alpha value of the seasoning curve function.
    #' @param Turnover.beta a numeric value.  The beta value of the seasoning curve function.
    #' @param Turnover.theta a numeric value. The theta value of the seasoning curve function.
    #' @param Seasonality.alpha a numeric value. The alpha value of the seasonality function.
    #' @param Seasonality.theta a numeric value. The beta value of the seasonality function.
    #' @param Incentive.Fast.theta.1 a numeric value. The first theta value of the fast payer S-curve.
    #' @param Incentive.Fast.theta.2 a numeric value. The second theta value of fast payer S-curve.
    #' @param Incentive.Fast.beta a numeric value. The beta value of the fast payer S-curve.
    #' @param Incentive.Fast.eta a numeric value.  The eta value of the fast payer S-curve.
    #' @param Incentive.Slow.theta.1 a numeric value. The first theta value of the slow payer S-curve.
    #' @param Incentive.Slow.theta.2 a numeric value. The second theta value of the slow payer S-curve.
    #' @param Incentive.Slow.beta a numeric value.  The beta value of the slow payer S-curve.
    #' @param Incentive.Slow.eta a numeric value. The eta value of the slow payer S-curve.
    #' @param Burnout.beta.1 a numeric value. The burnout first beta value to assign fast and slow payer.
    #' @param Burnout.beta.2 a numeric value. The burnout second beta value to assign fast and slow payer.
    #' @param BeginCDR a numeric value. The beginning CDR ramp.
    #' @param PeakCDR a numeric value. The peak of the CDR ramp.
    #' @param EndCDR a numeric value. The end value of the CDR ramp.
    #' @param PeakMonth a numeric value. The month at which the CDR ramp reaches its peak.
    #' @param PlateauMonths a numeric value. The number of months the CDR remains at its peak value.
    #' @param EndMonth a numeric value. The month at which the CDR ramp peak ends.
    #' @param MinOrigLTV a numeric value. The minimum original LTV which ties the 
    #' end minimum point of the orginal LTV multiplier function
    #' @param MaxOrigLTV a numeric value. The maximum orginal LTV which ties the
    #' maxumum point of the original LTV multiplier
    #' @param MinOrigMultiplier a numeric value. The minimum OLTV multiplier applied to the baseline
    #' @param MaxOrigMultiplier a numeric value. The maximum OLTV multipler applied to the baseline
    #' @param UpdatedLTV.beta a numeric value,  The beta value of the update loan to value
    #' @param SATO.beta A numeric value, the beta value of SATO
    #' @examples 
    #' \dontrun{
    #'  MakeModelTune(ModelName = "FH30.Generic_test1",
    #'  TurnoverRate = 0.08,
    #'  Turnover.alpha = 1.0,
    #'  Turnover.beta = 0.87,
    #'  Turnover.theta = 0.192,
    #'  Seasonality.alpha = 0.15,
    #'  Seasonality.theta = 12.0,
    #'  Incentive.Fast.theta.1 = 0.025,
    #'  Incentive.Fast.theta.2 = 0.019,
    #'  Incentive.Fast.beta = -4.0,
    #'  Incentive.Fast.eta = 1.0,
    #'  Incentive.Slow.theta.1 = 0.001,
    #'  Incentive.Slow.theta.2 = 0.004,
    #'  Incentive.Slow.beta = -1.0,
    #'  Incentive.Slow.eta = 0.5,
    #'  Burnout.beta.1 = -.01,
    #'  Burnout.beta.2 = -.01,
    #'  BeginCDR = 0,
    #'  PeakCDR = 2.75,
    #'  EndCDR = 1.5,
    #'  PeakMonth = 42,
    #'  PlateauMonths = 30,
    #'  EndMonth = 120,
    #'  MinOrigLTV = 80,
    #'  MaxOrigLTV = 90,
    #'  MinOrigMultiplier = 0.30,
    #'  MaxOrigMultiplier = 1.25,
    #'  UpdatedLTV.beta = 5,
    #'  SATO.beta = .8)
    #' }
    #' @export
    MakeModelTune <- function(
      ModelName = "character",
      TurnoverRate = "numeric",
      Turnover.alpha = "numeric",
      Turnover.beta = "numeric",
      Turnover.theta = "numeric",
      Seasonality.alpha = "numeric",
      Seasonality.theta = "numeric",
      Incentive.Fast.theta.1 = "numeric",
      Incentive.Fast.theta.2 = "numeric",
      Incentive.Fast.beta = "numeric",
      Incentive.Fast.eta = "numeric",
      Incentive.Slow.theta.1 = "numeric",
      Incentive.Slow.theta.2 = "numeric",
      Incentive.Slow.beta = "numeric",
      Incentive.Slow.eta = "numeric",
      Burnout.beta.1 = "numeric",
      Burnout.beta.2 = "numeric",
      BeginCDR = "numeric",
      PeakCDR = "numeric",
      EndCDR = "numeric",
      PeakMonth = "numeric",
      PlateauMonths = "numeric",
      EndMonth = "numeric",
      MinOrigLTV = "numeric",
      MaxOrigLTV = "numeric",
      MinOrigMultiplier = "numeric",
      MaxOrigMultiplier = "numeric",
      UpdatedLTV.beta = "numeric",
      SATO.beta = "numeric")
    {
     temp <- ModelTuningParam(
       TurnoverRate = TurnoverRate,
       Turnover.alpha = Turnover.alpha,
       Turnover.beta = Turnover.beta,
       Turnover.theta = Turnover.theta,
       Seasonality.alpha = Seasonality.alpha,
       Seasonality.theta = Seasonality.theta,
       Incentive.Fast.theta.1 = Incentive.Fast.theta.1,
       Incentive.Fast.theta.2 = Incentive.Fast.theta.2,
       Incentive.Fast.beta = Incentive.Fast.beta,
       Incentive.Fast.eta = Incentive.Fast.eta,
       Incentive.Slow.theta.1 = Incentive.Slow.theta.1,
       Incentive.Slow.theta.2 = Incentive.Slow.theta.2,
       Incentive.Slow.beta = Incentive.Slow.beta,
       Incentive.Slow.eta = Incentive.Slow.eta,
       Burnout.beta.1 = Burnout.beta.1,
       Burnout.beta.2 = Burnout.beta.2,
       BeginCDR = BeginCDR,
       PeakCDR = PeakCDR,
       EndCDR = EndCDR,
       PeakMonth = PeakMonth,
       PlateauMonths = PlateauMonths,
       EndMonth = EndMonth,
       MinOrigLTV = MinOrigLTV,
       MaxOrigLTV = MaxOrigLTV,
       MinOrigMultiplier = MinOrigMultiplier,
       MaxOrigMultiplier = MaxOrigMultiplier,
       UpdatedLTV.beta = UpdatedLTV.beta,
       SATO.beta = SATO.beta
       )
     
     SaveModelTune(ModelFile = temp, ModelName = ModelName)
     
     }
    
     
     
    
    