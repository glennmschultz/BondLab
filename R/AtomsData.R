  # Bond Lab is a software application for the analysis of 
  # fixed income securities. it provides a suite of applications
  # for fixed income analysis.  In addition bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # File License
  # Copyright (C) 2014  Glenn M Schultz, CFA

  #' @include PassThroughConstructor.R MortgageCashFlow.R MortgageCashFlowArray.R MortgageKeyRate.R MortgageScenario.R
  NULL
  
  # ------------------------------------------------------------------------------------------
  # The following classes define the ATOMs Index Analytics classes
  # ------------------------------------------------------------------------------------------
  
  #' An S4 classs representing the results of the ATOMs analytics
  #' 
  #' @slot ISpread a numeric value the interpolated spread over the U.S. Treasury coupon curve using consensus PSA
  #' @slot NSpread a numeric value the interpolated spread over the Swap coupon curve using consensus PSA
  #' @slot ZSpread a numeric value the spread over the U.S. Treasury spot rate curve
  #' @slot ModDuration a numeric value the modified duration based on the prepayment model vector
  #' @slot Convexity a numeric value the convexity based on the prepayment model vector
  #' @slot EffDuration a numeric value the sum of the key rate durations
  #' @slot EffConvexity a numeric value the 
  #' @slot KeyRateTenor a length 11 numeric vector the tenors of the key rates
  #' @slot KeyRateDuration a length 11 numeric vector the key rate durations
  #' @slot KeyRateConvexity a length 11 numeric vector the key rate convexities
  #' @slot OAD a numeric value the option adjusted duration
  #' @slot OAC a numeric value the option adjusted convexity
  #' @slot USTOAS a numeric value the OAS to the U.S. Treasury curve
  #' @slot USTZVSpread a numeric value the zero volatltity OAS to the U.S.Treasury curve
  #' @slot LIBOROAS a numeric value the OAS to the swap curve
  #' @slot LIBORZVSpread a numeric value the zero volatility OAS to the swap curve
  #' @exportClass AtomsData
  setClass("AtomsData",
           representation(
             ISpread = "numeric",
             NSpread = "numeric",
             ZSpread = "numeric",
             ModDuration = "numeric",
             Convexity = "numeric",
             EffDuration = "numeric",
             EffConvexity = "numeric",
             KeyRateTenor = "numeric",
             KeyRateDuration = "numeric",
             KeyRateConvexity = "numeric",
             OAD = "numeric",
             OAC = "numeric",
             USTOAS = "numeric",
             USTZVSpread = "numeric",
             LIBOROAS = "numeric",
             LIBORZVSpread = "numeric"))
  
  #' An S4 class representing ATOMs Scenario results
  #' 
  #' contains the class Scenario which is defined in the file
  #' MtgScenario.R
  #' @slot HorizonReturn a numeric value the horizon rate of return
  #' @slot contains Scenario class this class is set in MtgScenario.R
  #' @exportClass AtomsScenario
  setClass("AtomsScenario",
           representation(
             HorizonReturn = "numeric",
             contains = "Scenario"))
  
  #' An S4 class representing ATOMs analytics
  #' 
  #' The class is a superclass containing AtomsData class
  #' and the AtomsScenario class.  The AtomsScenario class
  #' is a superclass which contains the class Scenario defined
  #' in the file MtgScenario
  #' @exportClass AtomsAnalytics
  setClass("AtomsAnalytics",
           representation(),
           contains = c("AtomsData",
                        "AtomsScenario"))


  setGeneric("AtomsData",function(bond.id = "character",
                                  original.bal = numeric(),
                                  price = numeric(),
                                  trade.date = "character",
                                  settlement.date = "character",
                                  swapspread = list(),
                                  paths = "numeric",
                                  volatility = "numeric",
                                  consensusPSA = "numeric")
  {standardGeneric("AtomsData")})
  
  setMethod("initialize",
           signature("AtomsData"),
           function(.Object,
                    ISpread = "numeric",
                    NSpread = "numeric",
                    ZSpread = "numeric",
                    ModDuration = "numeric",
                    Convexity = "numeric",
                    EffDuration = "numeric",
                    EffConvexity = "numeric",
                    KeyRateTenor = "numeric",
                    KeyRateDuration = "numeric",
                    KeyRateConvexity = "numeric",
                    OAD = "numeric",
                    OAC = "numeric",
                    USTOAS = "numeric",
                    USTZVSpread = "numeric",
                    LIBOROAS = "numeric",
                    LIBORZVSpread = "numeric"){
           
                    .Object@ISpread = ISpread
                    .Object@NSpread = NSpread
                    .Object@ZSpread = ZSpread
                    .Object@ModDuration = ModDuration
                    .Object@Convexity = Convexity
                    .Object@EffDuration = EffDuration
                    .Object@EffConvexity = EffConvexity
                    .Object@KeyRateTenor = KeyRateTenor
                    .Object@KeyRateDuration = KeyRateDuration
                    .Object@KeyRateConvexity = KeyRateConvexity
                    .Object@OAD = OAD
                    .Object@OAC = OAC
                    .Object@USTOAS = USTOAS
                    .Object@USTZVSpread = USTZVSpread
                    .Object@LIBOROAS = LIBOROAS
                    .Object@LIBORZVSpread = LIBORZVSpread
          
                    return(.Object)
                    })
 
  #' A Function to calculate ATOMs Index Analytics
  #' 
  #' AtomsAnalytics is the analytic function for the ATOMs Index
  #' @param bond.id A character vector the id or cusip of the bond
  #' @param original.bal A numeric value the original balance
  #' @param price A numeric value the price of the pass-through security
  #' @param trade.date A numeric value the trade date
  #' @param settlement.date A numeric value the settlement date
  #' @param swapspread A list the of swap spreads length of which matches swap rate curve
  #' @param paths A numeric value the number of simulation paths
  #' @param volatility A numeric value the annualized volatility
  #' @param consensusPSA A numeric value the consensus PSA speed
  #' @export AtomsData
  AtomsData <- function(bond.id = "character",
                             original.bal = numeric(),
                             price = numeric(),
                             trade.date = "character",
                             settlement.date = "character",
                             swapspread = list(),
                             paths = "numeric",
                             volatility = "numeric",
                             consensusPSA = "numeric"){
  
  # -----------------------------------------------------------------------------
  # de-annualize volatility for the OAS model
  # -----------------------------------------------------------------------------
  volatility <- volatility/sqrt(trading.days)
   
  # -----------------------------------------------------------------------------
  # Define PPC Ramp for ATOMs ZSpread Calculation
  # -----------------------------------------------------------------------------
  BeginCPR <- .002 * (consensusPSA/PSA.basis)
  EndCPR <- .06 * (consensusPSA/PSA.basis)
  SeasoningPeriod = 30
   
  # -----------------------------------------------------------------------------
  # call security data, coupon curve data, and mortgage rate model
  # -----------------------------------------------------------------------------
  Bond.id = MBS(MBS.id = bond.id)
  LIBORCurve <- Rates(trade.date = trade.date)
  USTCurve <- LIBORCurve
  USTCurve[1,2:length(USTCurve[1,])] <-
  USTCurve[1,2:length(USTCurve[1,])] - as.numeric(swapspread)
  Rate.Delta = rate.delta
  MortgageRate <- MtgRate()
  principal <- original.bal * Bond.id@MBSFactor
  proceeds <- principal * (price/price.basis)
  LIBOR.short.rate <- LIBORCurve[1,2]
  nextpmtdate <- as.Date(Bond.id@NextPmtDate, format = "%m-%d-%Y")
  enddate <- as.Date(Bond.id@FinalPmtDate, format = "%m-%d-%Y")
  delay = Bond.id@PaymentDelay
   
  # ----------------------------------------------------------------------------
  # Call the prepayment model tuning parameters and 
  # instanitate the burnout variable
  # ----------------------------------------------------------------------------
  ModelTune <- ModelTune(bond.id = Bond.id)
  Burnout <- Bond.id@Burnout
  
  # ----------------------------------------------------------------------------
  # Calculate the LIBOR term strucuture using the CIR model
  # I need to decide how I want going to pass the swap spread data to the CIR model
  # this needs to be UST curve fit
  # ----------------------------------------------------------------------------
   
  Market.Fit <- CalibrateCIR(trade.date = trade.date, 
                              sigma = volatility)
  kappa_U  = Market.Fit$p1
  lambda_U = Market.Fit$p2
  theta_U  = Market.Fit$p3
   
  # ----------------------------------------------------------------------------
  # Zero volatility single path CIR model returns the forward rate curve
  # This is needed to drive the prepayment model for the calculation of the  
  # UST Z-spread calculation needs to fit 
  # ----------------------------------------------------------------------------
  CIRFwd <- CIRSim(shortrate = LIBOR.short.rate/yield.basis,
                    kappa = kappa_U,
                    theta = theta_U,
                    T = 40,
                    step = 1/months.in.year,
                    sigma = 0,
                    N = 1)
   
  # ----------------------------------------------------------------------------
  # Calculate the spot rate from the forward curve
  # ----------------------------------------------------------------------------
  CIRFwdLen <- length(CIRFwd) 
  CIRSpot <- ((1+CIRFwd[2:CIRFwdLen])/(1+CIRFwd[1:CIRFwdLen -1]) ^ (1/months.in.year))-1
  
  # ------------------------------------------------------------------------------
  # Calculate the forward rates
  # ------------------------------------------------------------------------------
  TwoYrFwd <- as.vector(CIRBondPrice(shortrate = CIRSpot, 
                                    kappa = kappa_U, 
                                    lambda = lambda_U, 
                                    theta = theta_U, 
                                    sigma = 0, 
                                    T = 2, 
                                    step = 0, 
                                    result = "y") * yield.basis)
   
  TenYrFwd <- as.vector(CIRBondPrice(shortrate = CIRSpot, 
                                      kappa = kappa_U, 
                                      lambda = lambda_U, 
                                      theta = theta_U, 
                                      sigma = 0, 
                                      T = 10, 
                                      step = 0, 
                                      result = "y") * yield.basis)
   
  # time vectors for discounting
  # time vectors for discounting are normally calculated from
  # bond payment data but for key rate they are manually calculated
  # a 30-year bond requires a 480 month forward curve to drive the prepayment model
   
  time.period <- seq(from = 1, to = CIRFwdLen, by = 1)
  time.period <- time.period/12
  timelength <- length(time.period)
  pmtdate <- as.character(as.Date(seq(nextpmtdate, enddate, by = "1 month"), "%m-%d-%Y") + delay)
  
   
  LIBORTermStructure <- new("TermStructure",
                           tradedate = as.character(trade.date),
                           period = time.period[2:timelength],
                           date = pmtdate,
                           spotrate = CIRSpot * yield.basis,
                           forwardrate = CIRFwd[2:CIRFwdLen],
                           TwoYearFwd = TwoYrFwd,
                           TenYearFwd = TenYrFwd)
   
  # ----------------------------------------------------------------------------
  # Fit the UST treasury curve using the single Factor CIR Model
  # ----------------------------------------------------------------------------
  # {Model fit code here}
   
  # ----------------------------------------------------------------------------
  # Run the prepayment model compute mortgage cash flow duration convexity
  # key rate duration and effective convexity
  # ---------------------------------------------------------------------------
  PrepaymentAssumption <- PrepaymentAssumption(
     bond.id = Bond.id,
     TermStructure = LIBORTermStructure,
     MortgageRate = MortgageRate,
     ModelTune = ModelTune,
     Burnout = Burnout,
     PrepaymentAssumption = "MODEL")
   
  ConsensusPSA <- PrepaymentAssumption(
     bond.id = Bond.id,
     TermStructure = LIBORTermStructure,
     MortgageRate = MortgageRate,
     ModelTune = ModelTune,
     Burnout = Burnout,
     PrepaymentAssumption = "PPC",
     begin.cpr = BeginCPR,
     end.cpr = EndCPR,
     seasoning.period = SeasoningPeriod)
   
   
  MortgageCashFlow <- MortgageCashFlow(bond.id = Bond.id,
                                        original.bal = original.bal,
                                        settlement.date = settlement.date,
                                        price = price,
                                        PrepaymentAssumption = PrepaymentAssumption)
   
  MortgageCashFlowPSA <- MortgageCashFlow(bond.id = Bond.id,
                                            original.bal = original.bal,
                                            settlement.date = settlement.date,
                                            price = price,
                                            PrepaymentAssumption = ConsensusPSA)
   
  
   
  # --------------------------------------------------------------------------------
  # Compute the spread to the LIBOR curve ATOMs NSpread
  # --------------------------------------------------------------------------------
  MtgYTM <- MortgageCashFlowPSA@YieldToMaturity * yield.basis
   
  InterpolateLIBOR <- loess(as.numeric(LIBORCurve[1,2:12]) ~
                                as.numeric(LIBORCurve[2,2:12]),
                                data = data.frame(LIBORCurve))
   
  SpreadToLIBOR <- (MtgYTM -
                       predict(InterpolateLIBOR, MortgageCashFlow@WAL))
   
  # --------------------------------------------------------------------------------
  # Compute the spread to the UST curve ATOMs ISpread
  # --------------------------------------------------------------------------------
  InterpolateUST <- loess(as.numeric(USTCurve[1,2:12]) ~
                                as.numeric(USTCurve[2,2:12]),
                                data = data.frame(USTCurve))
   
  SpreadToUST <- (MtgYTM -
                     predict(InterpolateUST, MortgageCashFlow@WAL))
   
  
  # -----------------------------------------------------------------------------------------
  # Compute the ZSpread - The Atoms ZSpread is the spread to the UST spot curve using
  # consensus PSA speed.  Need to fit the CIR model to UST
  # ----------------------------------------------------------------------------------------
  cashflow.length <- length(MortgageCashFlow@TotalCashFlow)
  SpotSpread <- function(spread = numeric(), 
                           cashflow = vector(), 
                           discount.rates = vector(), 
                           t.period = vector(), 
                           proceeds = numeric()){
      
  Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
  return(proceeds - Present.Value)}
    
  SolveSpotSpread <- uniroot(SpotSpread, 
                             interval = c(-.75, .75), 
                             tol = tolerance, 
                             cashflow = MortgageCashFlowPSA@TotalCashFlow,
                             discount.rates = LIBORTermStructure@spotrate[1:cashflow.length]/yield.basis, 
                             t.period = LIBORTermStructure@period[1:cashflow.length]/months.in.year , 
                             proceeds = proceeds)$root
    
  Zspread <- SolveSpotSpread * yield.basis
    
  # ------------------------------------------------------------------------------------------
  # LIBOR OAS Analysis
  # ------------------------------------------------------------------------------------------

  LIBOR.OAS <- Mortgage.OAS(bond.id = bond.id,
                            trade.date = trade.date,
                            settlement.date = settlement.date,
                            original.bal = original.bal,
                            price = price,
                            sigma = volatility,
                            paths = paths)
    
  LIBOROAS <- LIBOR.OAS@OAS * yield.basis
  LIBORZVSpread <- LIBOR.OAS@ZVSpread * yield.basis
    
    
  
  new("AtomsData",
      ISpread = SpreadToUST,
      NSpread = SpreadToLIBOR,
      ZSpread = Zspread,
      ModDuration = MortgageCashFlow@ModDuration,
      Convexity = MortgageCashFlow@Convexity,
      EffDuration = LIBOR.OAS@EffDuration,
      EffConvexity = LIBOR.OAS@EffConvexity,
      KeyRateTenor = LIBOR.OAS@KeyRateTenor,
      KeyRateDuration = LIBOR.OAS@KeyRateDuration,
      KeyRateConvexity = LIBOR.OAS@KeyRateConvexity,
      OAD = 9999,
      OAC = 9999,
      USTOAS = 9999,
      USTZVSpread = 9999,
      LIBOROAS = LIBOR.OAS@OAS,
      LIBORZVSpread = LIBORZVSpread)
  }