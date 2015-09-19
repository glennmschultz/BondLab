  # Bond Lab is a software application for the analysis of 
  # fixed income securities. it provides a suite of applications
  # for fixed income analysis.  In addition bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Glenn M Schultz, CFA

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
                    callNextMethod(.Object,...)})
 
 #' A Function to calculate ATOMs Index Analytics
 #' 
 #' AtomsAnalytics is the analytic function for the ATOMs Index
 #' @param bond.id A character vector the id or cusip of the bond
 #' @param original.bal A numeric value the original balance
 #' @param price A numeric value the price of the pass-through security
 #' @param trade.date A numeric value the trade date
 #' @param settlement.date A numeric value the settlement date
 #' @param swapspread A list the of swap spreads length of which matches swap rate curve
 #' @param method A character vector the method used to fit the term structure
 #' @param paths A numeric value the number of simulation paths
 #' @param volatility A numeric value the annualized volatility
 #' @param consensusPSA A numeric value the consensus PSA speed
 #' @export
 AtomsData <- function(bond.id = "character",
                             original.bal = numeric(),
                             price = numeric(),
                             trade.date = "character",
                             settlement.date = "character",
                             swapspread = list(),
                             method = "character",
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
   
   # ----------------------------------------------------------------------------
   # Call the prepayment model tuning parameters and 
   # instanitate the burnout variable
   # ----------------------------------------------------------------------------
   ModelTune <- ModelTune(bond.id = Bond.id)
   Burnout <- Bond.id@Burnout
  
   # ----------------------------------------------------------------------------
   # Calculate the term strucutre
   # ----------------------------------------------------------------------------
   
   LIBORTermStructure <- TermStructure(rates.data = LIBORCurve, method = method)
   USTTermStructure <- TermStructure(rates.data = USTCurve, method = method)
   
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
     seasoning.period = SeasoningPeriod
     )
   
   
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
   
   
   MortgageTermStructure <- MtgTermStructure(bond.id = Bond.id,
                               original.bal = original.bal,
                               Rate.Delta = Rate.Delta,
                               TermStructure = LIBORTermStructure,
                               settlement.date = settlement.date,
                               principal = principal,
                               price = price,
                               cashflow = MortgageCashFlow)
   
   # --------------------------------------------------------------------------------
   # Compute the spread to the LIBOR curve ATOMs NSpread
   # --------------------------------------------------------------------------------
   MtgYTM <- MortgageCashFlow@YieldToMaturity * yield.basis
   
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
    # consensus PSA speed
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
                               discount.rates = USTTermStructure@spotrate[1:cashflow.length]/yield.basis, 
                               t.period = USTTermStructure@period[1:cashflow.length]/months.in.year , 
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
        EffDuration = MortgageTermStructure@EffDuration,
        EffConvexity = MortgageTermStructure@EffConvexity,
        KeyRateTenor = MortgageTermStructure@KeyRateTenor,
        KeyRateDuration = MortgageTermStructure@KeyRateDuration,
        KeyRateConvexity = MortgageTermStructure@KeyRateConvexity,
        OAD = 9999,
        OAC = 9999,
        USTOAS = 9999,
        USTZVSpread = 9999,
        LIBOROAS = LIBOROAS,
        LIBORZVSpread = LIBORZVSpread)
 }