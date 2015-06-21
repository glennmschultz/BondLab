# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

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
                   SATO.beta = "numeric"){
            
            .Object@TurnoverRate = TurnoverRate
            .Object@Turnover.alpha = Turnover.alpha
            .Object@Turnover.beta = Turnover.beta
            .Object@Turnover.theta = Turnover.theta
            .Object@Seasonality.alpha = Seasonality.alpha
            .Object@Seasonality.theta = Seasonality.theta
            .Object@Incentive.Fast.theta.1 = Incentive.Fast.theta.1
            .Object@Incentive.Fast.theta.2 = Incentive.Fast.theta.2
            .Object@Incentive.Fast.beta = Incentive.Fast.beta
            .Object@Incentive.Fast.eta = Incentive.Fast.eta
            .Object@Incentive.Slow.theta.1 = Incentive.Slow.theta.1
            .Object@Incentive.Slow.theta.2 = Incentive.Slow.theta.2
            .Object@Incentive.Slow.beta = Incentive.Slow.beta
            .Object@Incentive.Slow.eta = Incentive.Slow.eta
            .Object@Burnout.beta.1 = Burnout.beta.1
            .Object@Burnout.beta.2 = Burnout.beta.2
            .Object@BeginCDR = BeginCDR
            .Object@PeakCDR = PeakCDR
            .Object@EndCDR = EndCDR
            .Object@PeakMonth = PeakMonth
            .Object@PlateauMonths = PlateauMonths
            .Object@EndMonth = EndMonth
            .Object@MinOrigLTV = MinOrigLTV
            .Object@MaxOrigLTV = MaxOrigLTV
            .Object@MinOrigMultiplier = MinOrigMultiplier
            .Object@MaxOrigMultiplier = MaxOrigMultiplier
            .Object@UpdatedLTV.beta = UpdatedLTV.beta
            .Object@SATO.beta = SATO.beta
            
            return(.Object)
            callNextMethod(.Object,...)
          })

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
       
     #ModelTuneConn <- gzfile(description = paste("~/BondLab/PrepaymentModel/", ModelName, ".rds", sep =""))
     #saveRDS(temp, ModelTuneConn)
     #close(ModelTuneConn)
     }
    
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
     
    
    