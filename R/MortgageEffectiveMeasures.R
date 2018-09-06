
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.


  # The following script is used to calculate the effective duration and 
  # convexity metrics for mortgage backed securities.  To create the script
  # the standard procedure is followed set class, set generics,
  # set methods, functions.  This function calculates the efffective duration
  # and convexity by shifting the spot rate curve.

  #' @include MortgageKeyRate.R
  NULL
  
  #' An S4 class MortgageEffectiveMeasures
  #' 
  #' @slot EffDuration A numeric value the effective duration
  #' @slot EffConvexity A numeric value the effective convexity
  #' @exportClass MortgageEffectiveMeasures
  setClass("MortgageEffectiveMeasures",
           representation(EffDuration = "numeric", 
                          EffConvexity = "numeric"
         ))
  
  # Note: generic EffDuration is set in MortgageKeyRate.R
  # Note: generic EffConvexity is set in MortgageKeyRate.R
  
  
  setMethod("initialize",
            signature("MortgageEffectiveMeasures"),
            function(.Object,
                     EffDuration = "numeric",
                     EffConvexity = "numeric",
                     ...){
              callNextMethod(.Object,
                             EffDuration = EffDuration,
                             EffConvexity = EffConvexity,
                             ...)
            })
  
  #' A method to extract the EffDuration from class MortgageEffectiveMeasures
  #' 
  #' @param object The name of the S4 object of type MortgageEffectiveMeasures
  #' @exportMethod EffDuration
  setMethod("EffDuration", signature("MortgageEffectiveMeasures"),
            function(object){object@EffDuration})
  
  #' A method to extract the EffConvexity from class MortgageEffectiveMeasures
  #' 
  #' @param object The name of the S4 object of type MortgageEffectiveMeasures
  #' @exportMethod EffConvexity
  setMethod("EffConvexity", signature("MortgageEffectiveMeasures"),
            function(object){object@EffConvexity})

  #' A function to calculate mortgage effective duration and convexity
  #' 
  #' @param bond.id A character string referencing  an object of type bond.id
  #' @param settlement.date A character string the settlement date
  #' @param rates.data A character string referencing and object rates
  #' @param TermStructure A character string referencing an object of type
  #' TermStructure
  #' @param price A character the price of the MBS.  Price may be entered in
  #' decimal notation or 32nds notation
  #' @export MortgageEffectiveMeasures
  MortgageEffectiveMeasures <- function(
    bond.id = "character",
    settlement.date = "character",
    rates.data = "character",
    TermStructure = "character",
    price = "character"){
  
  rates.data <- rates.data
  TermStructure = TermStructure
  MortgageRate = MortgageRate()
  Price = PriceTypes(price = price)
  proceeds <- OriginalBal(bond.id) * MBSFactor(bond.id) * PriceBasis(Price)
  
  # Pricing prepayment vector and cashflows under the no change scenario
  prepayment <- PrepaymentModel(
    bond.id = bond.id,
    TermStructure = TermStructure,
    MortgageRate = MortgageRate,
    ModelTune = ModelTune(bond.id = bond.id),
    Burnout = BurnOut(bond.id),
    PrepaymentAssumption = "MODEL")
  
  CashFlow <-MortgageCashFlow(
    bond.id = bond.id,
    original.bal = OriginalBal(bond.id),
    settlement.date = settlement.date,
    price = PriceDecimalString(Price),
    PrepaymentAssumption = prepayment)
  
  CurveSpreads <- CurveSpreads(
    rates.data = rates.data,
    CashFlow = CashFlow,
    TermStructure = TermStructure,
    proceeds = proceeds)
  
  #Call the RateDelta Scenarios the basis shift is 25 basis points
  ScenarioUp <- ScenarioCall("U25s")
  ScenarioDwn <- ScenarioCall("D25s")
  
  # Set termstructure up and termstructure down objects equal to the term
  # structure object.
  TermStructureUp <- TermStructure
  TermStructureDwn <- TermStructure
  
  # Apply the scenario shift up to the spot and forward rates
  SpotRate(TermStructureUp) <-  
    ScenarioFormula(ScenarioUp)(rates.data = SpotRate(TermStructure),
                                Shiftbps = Shiftbps(ScenarioUp))
  ForwardRate(TermStructureUp) <-
    ScenarioFormula(ScenarioUp)(rates.data = ForwardRate(TermStructure),
                                Shiftbps = Shiftbps(ScenarioUp))
  
  # Compute Forward Rates for the prepayment model
  TwoYearForward(TermStructureUp) <- Forward.Rate(
    term.structure = TermStructureUp, forward.tenor = 24, type = "C")
  TenYearForward(TermStructureUp) <- Forward.Rate(
    term.structure = TermStructureDwn, forward.tenor = 120, type = "C")
  
  # Model the prepayment up vector
  prepaymentup <- PrepaymentModel(
    bond.id = bond.id,
    TermStructure = TermStructureUp,
    MortgageRate = MortgageRate,
    ModelTune = ModelTune(bond.id = bond.id),
    Burnout = BurnOut(bond.id),
    PrepaymentAssumption = "MODEL")
  
  CashFlowup <-CashFlowEngine(
    bond.id = bond.id,
    settlement.date = settlement.date,
    principal = OriginalBal(bond.id) * MBSFactor(bond.id),
    PrepaymentAssumption = prepaymentup)
  
  # Calculate the discount rates used 
  CashFlowLen <- nrow(CashFlowup)
  
  DiscountRate = (SpotRate(TermStructureUp)[1:CashFlowLen] + 
                    ZeroVolSpread(CurveSpreads))
  DiscountRate = (1 + (DiscountRate/yield.basis))^
    (-CashFlowup[1:CashFlowLen,"Time"])
  
  DiscCashFlow <- CashFlowup[,"Investor CashFlow"] * DiscountRate
  DiscCashFlow <- sum(DiscCashFlow)
  
  PriceUp <- DiscCashFlow
  
  # Apply the down scenario shift down to the spot and forward rates
  SpotRate(TermStructureDwn) <- 
    ScenarioFormula(ScenarioDwn)(rates.data = SpotRate(TermStructure),
                                 Shiftbps = Shiftbps(ScenarioDwn))
  ForwardRate(TermStructureDwn) <-
    ScenarioFormula(ScenarioDwn)(rates.data = ForwardRate(TermStructure),
                                 Shiftbps = Shiftbps(ScenarioDwn))
  # Compute Forward Rates
  TwoYearForward(TermStructureDwn) <- Forward.Rate(term.structure = TermStructureDwn, forward.tenor = 24, type = "C")
  TenYearForward(TermStructureDwn) <- Forward.Rate(term.structure = TermStructureDwn, forward.tenor = 120, type = "C")
  
  # Prepayment Down Vector
  prepaymentdwn <- PrepaymentModel(
    bond.id = bond.id,
    TermStructure = TermStructureDwn,
    MortgageRate = MortgageRate,
    ModelTune = ModelTune(bond.id = bond.id),
    Burnout = BurnOut(bond.id),
    PrepaymentAssumption = "MODEL")
  
  CashFlowdwn <-CashFlowEngine(
    bond.id = bond.id,
    settlement.date = settlement.date,
    principal = OriginalBal(bond.id) * MBSFactor(bond.id),
    PrepaymentAssumption = prepaymentdwn)
  
  CashFlowLen <- nrow(CashFlowdwn)
  
  DiscountRate = (SpotRate(TermStructureDwn)[1:CashFlowLen] + 
                    ZeroVolSpread(CurveSpreads))
  DiscountRate = (1 + (DiscountRate/yield.basis))^
    (-CashFlowdwn[1:CashFlowLen,"Time"])
  
  DiscCashFlow <- CashFlowdwn[,"Investor CashFlow"] * DiscountRate
  DiscCashFlow <- sum(DiscCashFlow)
  
  PriceDwn <- DiscCashFlow
  Rate.Delta = rate.delta/yield.basis
  EffectiveDuration <- (PriceUp - PriceDwn) / (2 * proceeds * Rate.Delta)
  EffectiveConvexity <- (PriceUp + PriceDwn - (2 * proceeds)) /
    (2 * proceeds * Rate.Delta^2)
  
  new("MortgageEffectiveMeasures",
      EffDuration = EffectiveDuration,
      EffConvexity = EffectiveConvexity)
  }
