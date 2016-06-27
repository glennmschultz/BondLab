  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.

  #' @include ScenarioConstructor.R MortgageCashFlow.R TermStructure.R 
  #' @include PrepaymentModel.R MortgageKeyRate.R
  NULL
  
  
  #' An S4 Class A list of Scenario outcomes
  #'
  #' The class MtgScenarioSet is a list of classes Scenario
  #' @slot Scenario A list of the class Scenario
  #' @exportClass MtgScenarioSet
  setClass("MtgScenarioSet",
           representation(
             Scenario = "list"))
  
  setGeneric("MtgScenarioSet", function(Scenario = "list")
  {standardGeneric("MtgScenarioSet")})
  
  #' A standard generic function to extract the slot Scenario
  #' @param object An S4 class object of the type MtgScenarioSet
  #' @export
  setGeneric("ScenarioSet", function(object)
  {standardGeneric("ScenarioSet")})
  
  setMethod("initialize",
            signature("MtgScenarioSet"),
            function(.Object,
                     Scenario = "list",
                     ...)
            {
              callNextMethod(.Object,
                             Scenario = Scenario,
                             ...)
            }
  )
  
  #' A method to extract the Scenario list from S4 class MtgScenarioSet
  #' @param object the name of the S4 object of the type Scenario
  #' @exportMethod ScenarioSet
  setMethod("ScenarioSet", signature("MtgScenarioSet"),
            function(object){object@Scenario})

  #' An S4 Class representing the results of mortgage return scenario analysis
  #' 
  #' The class Mtg. Scenario holds the results of a scenario analysis run
  #' @slot Period A numeric value the period number forward from current
  #' @slot PmtDate A character the payment date on which the cashflow is 
  #' expected to be received
  #' @slot TimePeriod A numeric value the period stated as a unit of time
  #' @slot BeginningBal A numeric value the forecasted beginning balance 
  #' in the period
  #' @slot PassThroughInterest A numeric value the interest paid through 
  #' to the investor
  #' @slot ScheduledPrin A numeric value the scheduled principal due 
  #' in the period
  #' @slot PrepaidPrin A numeric value the forecasted prepaid principal 
  #' in the period
  #' @slot EndingBal A numeric value the forecasted ending balance 
  #' in the period
  #' @slot TotalCashFlow A numeric value the forecasted total cashflow 
  #' received by the investor in the period
  #' @slot SpotRate A numeric vector the spot rate curve
  #' @slot ForwardRate A numeric vector the forward rate curve
  #' @slot SMM A numeric vector the forecasted SMM vector
  #' @slot YieldToMaturity A numeric value the scenario yield to maturity
  #' @slot WAL A numeric value the scenario weighted average life
  #' @slot SpreadToInterCurve A numeric value the spread to the 
  #' interpolated curve
  #' @slot ModDuration A numeric value the scenario modified duration
  #' @slot Convexity A numeric value the scenario convexity
  #' @slot EffDuration A numeric value the effective duration which is 
  #' the sum of the key rate durations
  #' @slot EffConvexity A numeric value the effective duration which is 
  #' the sum of the key rate convexities
  #' @slot KeyRateTenor A numeric vector the key rate tenors
  #' @slot KeyRateDuration A numeric vector the duration of each key rate tenor
  #' @slot KeyRateConvexity A numeric vector the convexity of 
  #' each key rate tenor
  #' @slot HorizonReturn A numeric value the horizon total return
  #' @slot HorizonMos A numeric value the number of months to 
  #' the scenario horizon date
  #' @exportClass MtgScenario    
  setClass("MtgScenario",
           representation( 
             Period = "numeric",
             PmtDate = "character",
             TimePeriod = "numeric",
             BeginningBal = "numeric",
             PassThroughInterest = "numeric",
             ScheduledPrin = "numeric",
             PrepaidPrin = "numeric",
             EndingBal = "numeric",
             TotalCashFlow = "numeric",
             SpotRate = "numeric",
             ForwardRate = "numeric",
             SMM = "numeric",
             YieldToMaturity = "numeric",
             WAL = "numeric",
             SpreadToInterCurve = "numeric",
             ModDuration = "numeric",
             Convexity = "numeric", 
             EffDuration = "numeric",
             EffConvexity = "numeric",
             KeyRateTenor = "numeric",
             KeyRateDuration = "numeric",
             KeyRateConvexity = "numeric",
             HorizonReturn = "numeric",
             HorizonMos = "numeric"),
           contains = "Scenario")
  
  
  # Note: standard generic period is defined in MortgageCashFlow.R
  # Note: standard generic PmtDate is defined in MortgageCashFlow.R
  # Note: standard generic TimePeriod is defined in MortgageCashFlow.R
  # Note: standard generic BeginningBal is defined in MortgageCashFlow.R
  # Note: standard generic PassThroughInterest is defined in MortgageCashFlow.R
  # Note: standard generic ScheduledPrin is defined in MortgageCashFlow.R
  # Note: standard generic PrepaidPrin is defined in MortgageCashFlow.R
  # Note: standard generic EndingBal is defined in MortgageCashFlow.R
  # Note: standard generic TotalCashFlow is defined in MortgageCashFlow.R
  # Note: standard generic SpotRate is defined in MortgageCashFlow.R
  # Note: standard generic ForwardRate is defined in MortgageCashFlow.R
  # Note: standard generic SMM is defined in PrepaymentModel.R
  # Note: standard generic YieldToMaturity is defined in MortgageCashFlow.R
  # Note: standard generic WAL is defined in MortgageCashFlow.R
  # Note: standard generic ModDuration is defined in MortgageCashFlow.R
  # Note: standard generic Convexity is defined in MortgageCashFlow.R
  # Note: standard generic EffDuration is defined in MortgageKeyRate.R
  # Note: standard generic EffConvexity is defined in MortgageKeyRate.R
  
  setGeneric("MtgScenario", function(bond.id ="character",
                                     settlement.date = "character",
                                     rates.data = "character",
                                     price = numeric(), 
                                     original.bal = numeric(),
                                     scenario = "character",
                                     horizon.months = numeric(),
                                     method = "character",
                                     prepayment = "character",
                                     ...,
                                     horizon.spot.spread = NULL,
                                     horizon.nominal.spread = NULL,
                                     horizon.OAS = NULL,
                                     horizon.price = NULL,
                                     begin.cpr = numeric(),
                                     end.cpr = numeric(),
                                     seasoning.period = numeric(),
                                     CPR = numeric())
  {standardGeneric("MtgScenario")})
  
  #' A standard generic function to access the slot HorizonRetrun
  #' @param object An S4 object of type MtgScenario
  #' @export
  setGeneric("HorizonReturn", function(object)
  {standardGeneric("HorizonReturn")})
  
  #' A standard generic function to access the slot HorizonMos
  #' @param object An S4 object of type MtgScenario
  #' @export
  setGeneric("HorizonMos", function(object)
    {standardGeneric("HorizonMos")})
  
  #' A standard generic function to access the slot SpreadToInterCurve
  #' @param object An S4 object of type MtgScenario
  #' @export
  setGeneric("SpreadToInterCurve", function(object)
  {standardGeneric("SpreadToInterCurve")})
  
  # Note: standard generic function Name is defined above
  # Note: standard generic function Type is defined above
  # Note: standard generic function Horizon is defined above
  # Note: standard generic function ShiftType is defined above
  # Note: standard generic function Shiftbps is defined above
  # Note: standard generic function Formula is defined above
  
  
  setMethod("initialize",
            signature("MtgScenario"),
            function(.Object,
                     Period = numeric(),
                     PmtDate = "character",
                     TimePeriod = numeric(),
                     BeginningBal = numeric(),
                     PassThroughInterest = numeric(),
                     ScheduledPrin = numeric(),
                     PrepaidPrin = numeric(),
                     EndingBal = numeric(),
                     TotalCashFlow = numeric(),
                     SpotRate = numeric(),
                     ForwardRate = numeric(),
                     SMM = numeric(),
                     YieldToMaturity = numeric(),
                     WAL = numeric(),
                     SpreadToInterCurve = numeric(),
                     ModDuration = numeric(),
                     Convexity = numeric(), 
                     EffDuration = numeric(),
                     EffConvexity = numeric(),
                     KeyRateTenor = numeric(),
                     KeyRateDuration = numeric(),
                     KeyRateConvexity = numeric(),
                     HorizonReturn = numeric(),
                     HorizonMos = numeric(),
                     Name = "character",
                     Type = "character",
                     Horizon = "character",
                     ShiftType = "character",
                     Shiftbps = numeric(),
                     Formula = "function",
                     ...)
            {
              callNextMethod(.Object,
                             Period = Period,
                             PmtDate = PmtDate,
                             TimePeriod = TimePeriod,
                             BeginningBal = BeginningBal,
                             PassThroughInterest = PassThroughInterest,
                             ScheduledPrin = ScheduledPrin,
                             PrepaidPrin = PrepaidPrin,
                             EndingBal = EndingBal,
                             TotalCashFlow = TotalCashFlow,
                             SpotRate = SpotRate,
                             ForwardRate = ForwardRate,
                             SMM = SMM,
                             YieldToMaturity = YieldToMaturity,
                             WAL = WAL,
                             SpreadToInterCurve = SpreadToInterCurve,
                             ModDuration = ModDuration,
                             Convexity = Convexity, 
                             EffDuration = EffDuration,
                             EffConvexity = EffConvexity,
                             KeyRateTenor = KeyRateTenor,
                             KeyRateDuration = KeyRateDuration,
                             KeyRateConvexity = KeyRateConvexity,
                             HorizonReturn = HorizonReturn,
                             HorizonMos = HorizonMos,
                             Name = Name,
                             Type = Type,
                             Horizon = Horizon,
                             ShiftType = ShiftType,
                             Shiftbps = Shiftbps,
                             Formula = Formula,
                             ...)
            })
  
  #' A method to extract Period from S4 class MtgScenario
  #' @param object The name of an S4 class of type MtgScenario
  #' @exportMethod  Period
  setMethod("Period", signature("MtgScenario"),
            function(object){object@Period})
  
  #' A method to extract PmtDate from S4 class MtgScenario
  #' @param object The name of an S4 class of type MtgScenario
  #' @exportMethod PmtDate
  setMethod("PmtDate", signature("MtgScenario"),
            function(object){object@PmtDate})
  
  #' A method to extract TimePeriod from S4 class MtgScenario
  #' @param object The name of an S4 class of type MtgScenario
  #' @exportMethod TimePeriod
  setMethod("TimePeriod", signature("MtgScenario"),
            function(object){object@TimePeriod})
  
  #' A method to extract BeginningBal from S4 class MtgScenario
  #' @param object The name of an S4 class of type MtgScenario
  #' @exportMethod BeginningBal
  setMethod("BeginningBal", signature("MtgScenario"),
            function(object){object@BeginningBal})
  
  #' A method to extract PassThroughInterest from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod PassThroughInterest
  setMethod("PassThroughInterest", signature("MtgScenario"),
            function(object){object@PassThroughInterest})
  
  #' A method to extract ScheduledPrin from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod ScheduledPrin
  setMethod("ScheduledPrin", signature("MtgScenario"),
            function(object){object@ScheduledPrin})
  
  #' A method to extract PrepaidPrin from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod PrepaidPrin
  setMethod("PrepaidPrin", signature("MtgScenario"),
            function(object){object@PrepaidPrin})
  
  #' A method to extract EndingBalance from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod EndingBalance
  setMethod("EndingBalance", signature("MtgScenario"),
            function(object){object@EndingBal})
  
  #' A method to extract TotalCashFlow from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod TotalCashFlow
  setMethod("TotalCashFlow", signature("MtgScenario"),
            function(object){object@TotalCashFlow})
  
  #' A method to extract SpotRate curve from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod SpotRate
  setMethod("SpotRate", signature("MtgScenario"),
            function(object){object@SpotRate})
  
  #' A method to extract ForwardRate curve from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod ForwardRate
  setMethod("ForwardRate", signature("MtgScenario"),
            function(object){object@ForwardRate})
  
  #' A method to extract SMM from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod SMM
  setMethod("SMM", signature("MtgScenario"),
            function(object){object@SMM})
  
  #' A method to extract YieldToMaturity from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod YieldToMaturity
  setMethod("YieldToMaturity", signature("MtgScenario"),
            function(object){object@YieldToMaturity})
  
  #' A method to extract WAL from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod WAL
  setMethod("WAL", signature("MtgScenario"),
            function(object){object@WAL})
  
  #' A method to extract SpreadToInterCurve from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod SpreadToInterCurve
  setMethod("SpreadToInterCurve", signature("MtgScenario"),
            function(object){object@SpreadToInterCurve})
  
  #' A method to extract ModDuration from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod ModDuration
  setMethod("ModDuration", signature("MtgScenario"),
            function(object){object@ModDuration})
  
  #' A method to extract Convexity from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod Convexity
  setMethod("Convexity", signature("MtgScenario"),
            function(object){object@Convexity})
  
  #' A method to extract EffDuration from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod EffDuration
  setMethod("EffDuration", signature("MtgScenario"),
            function(object){object@EffDuration})
  
  #' A method to extract EffConvexity from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod EffConvexity
  setMethod("EffConvexity", signature("MtgScenario"),
            function(object){object@EffConvexity})
  
  #' A method to extract KeyRateTenor from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod KeyRateTenor
  setMethod("KeyRateTenor", signature("MtgScenario"),
            function(object){object@KeyRateTenor})
  
  #' A method to extract KeyRateDuration from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod KeyRateDuration
  setMethod("KeyRateDuration", signature("MtgScenario"),
            function(object){object@KeyRateDuration})
  
  #' A method to extract KeyRateConvexity from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod KeyRateConvexity
  setMethod("KeyRateConvexity", signature("MtgScenario"),
            function(object){object@KeyRateConvexity})
  
  #' A method to extract HorizonReturn from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod HorizonReturn
  setMethod("HorizonReturn", signature("MtgScenario"),
            function(object){object@HorizonReturn})
  
  #' A method to extract HorizonMos from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod HorizonMos
  setMethod("HorizonMos", signature("MtgScenario"),
            function(object){object@HorizonMos})
  
  #' A method to extract Name (scenario name from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod Name
  setMethod("Name", signature("MtgScenario"),
            function(object){object@Name})
  
  #' A method to extract Type (scenario type) from S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod Type
  setMethod("Type", signature("MtgScenario"),
            function(object){object@Type})
  
  #' A method to extract Horizon (horizon period) from an S4 class MtgScenario
  #' @param object the name of an S4 class of type of MtgScenario
  #' @exportMethod Horizon
  setMethod("Horizon", signature("MtgScenario"),
            function(object){object@Horizon})
  
  #' A method to extract ShiftType from an S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod ShiftType
  setMethod("ShiftType", signature("MtgScenario"),
            function(object){object@ShiftType})
  
  #' A method to extract Shiftbps from an S4 class MtgScenario
  #' @param object The name of an S4 class of type MtgScenario
  #' @exportMethod Shiftbps
  setMethod("Shiftbps", signature("MtgScenario"),
            function(object){object@Shiftbps})
  
  #' A method to extract Formula from an S4 class MtgScenario
  #' @param object the name of an S4 class of type MtgScenario
  #' @exportMethod ScenarioFormula
  setMethod("ScenarioFormula", signature("MtgScenario"),
            function(object){object@Formula})
  
  #---------------------------------------------------------
  # Scenario Total Return Analysis
  # Calls the bond.id and applies the scenario
  #---------------------------------------------------------
  
  #' Mortgage Scenario Analysis
  #' 
  #' A function to compute the total return of mortgage pass-throughs MBS
  #' @param bond.id A character string to object of type MBS details
  #' @param settlement.date A charcter string the settlement date
  #' @param rates.data A character string the trade date mm-dd-yyyy
  #' @param price A numeric value the price
  #' @param original.bal A numeric value the price
  #' @param scenario A character string the scenario
  #' @param horizon.months A numeric value the time horizon
  #' @param method A character string the method used to fit the term structure
  #' @param prepayment A character string the prepayment assumption
  #' @param ... Optional values when PSA or CPR is used
  #' @param horizon.spot.spread A numeric value the horizon spread
  #' @param horizon.nominal.spread A numeric value the horizon spread
  #' @param horizon.OAS A numeric value the horizon option adjusted spread
  #' @param horizon.price A numeric value the horizon price in decimal form
  #' @param begin.cpr A numeric value the beginning CPR value
  #' @param end.cpr A numeric value the ending CPR value
  #' @param seasoning.period A numeric value the length of the seasoning ramp
  #' @param CPR A numeric value the CPR speed
  #' @export MtgScenario
  MtgScenario <- function(bond.id ="character",
                          settlement.date = "character",
                          rates.data = "character",
                          price = numeric(), 
                          original.bal = numeric(),
                          scenario = "character",
                          horizon.months = numeric(),
                          method = "character",
                          prepayment = "character",
                          ...,
                          horizon.spot.spread = NULL,
                          horizon.nominal.spread = NULL,
                          horizon.OAS = NULL,
                          horizon.price = NULL,
                          begin.cpr = numeric(),
                          end.cpr = numeric(),
                          seasoning.period = numeric(),
                          CPR = numeric()) { 
    
    # ========================================================================
    # Mortgage Scenario analysis is done in two steps
    # The first is calculated the expected cash-flows received over the 
    # investment horizon The second is to "roll" the pass through security 
    # forward and price the expected future cash-flows
    # ========================================================================
    
    if(is.null(horizon.spot.spread) != TRUE) {
      horizon.price.type <- "spot"
    } else if(is.null(horizon.nominal.spread) != TRUE) {
      horizon.price.type <- "nominal"  
    } else if(is.null(horizon.OAS) != TRUE) {
      horizon.price.type <- "oas"  
    } else {
      horizon.price.type <- "price"
    }
    
    
    bond.id <- bond.id
    MortgageRate <- MtgRate()
    ModelTune <- ModelTune(bond.id = bond.id)
    Burnout = BurnOut(bond.id)
    Scenario <- ScenarioCall(Scenario = scenario)
    rates <- rates.data
    
    rates[1,2:length(rates)] <- 
      ScenarioFormula(Scenario)(rates[1,1:length(rates)], 
                                Shiftbps = Shiftbps(Scenario))
    
    TermStructure <- TermStructure(rates.data = rates, 
                                   method = method)
    
    
    Prepayment <- PrepaymentModel(bond.id = bond.id, 
                                  MortgageRate = MortgageRate, 
                                  TermStructure = TermStructure, 
                                  PrepaymentAssumption = prepayment, 
                                  ModelTune = ModelTune,
                                  Severity = 0,
                                  Burnout = Burnout, 
                                  begin.cpr = begin.cpr, 
                                  end.cpr = end.cpr, 
                                  seasoning.period = seasoning.period, 
                                  CPR = CPR)
    
    MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                         original.bal = original.bal, 
                                         settlement.date = settlement.date, 
                                         price = price, 
                                         PrepaymentAssumption = Prepayment)
    
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                        as.numeric(rates.data[2,2:12]), 
                        data = data.frame(rates.data))  
    
    SpreadtoCurve <- (YieldToMaturity(MortgageCashFlow) * yield.basis) - 
      predict(InterpolateCurve, WAL(MortgageCashFlow))
    
    proceeds <- Accrued(MortgageCashFlow) + (original.bal * 
                                  MBSFactor(bond.id) * (price/price.basis))
    principal <- original.bal * MBSFactor(bond.id)
    
    MortgageTermStructure <- MtgTermStructure(bond.id = bond.id, 
                                            original.bal = original.bal, 
                                            Rate.Delta = rate.delta, 
                                            TermStructure = TermStructure,
                                            settlement.date = settlement.date, 
                                            principal = principal, 
                                            price = price, 
                                            cashflow = MortgageCashFlow)
    
    # =========================================================================
    # Begin horizon mortgage pass-through analysis
    # =========================================================================
    
    HorizonCurve <- rates
    HorizonCurve[1,1] <- as.character(as.Date(HorizonCurve[1,1]) %m+% 
      months(horizon.months))
    HorizonSettlement <- as.Date(settlement.date, format = "%m-%d-%Y") %m+% 
      months(horizon.months)
    
    
    HorizonTermStructure <- TermStructure(rates.data = HorizonCurve,
                                          method = "ns")
    
    ForwardPassThrough(bond.id = bond.id,
                       original.bal = original.bal,
                       projected.cashflow = MortgageCashFlow,
                       horizon.months = horizon.months)

    HorizonMBS <- HorizonMBS()
    
    HorizonPrepaymentAssumption <- PrepaymentModel(bond.id = HorizonMBS,
                                          MortgageRate = MortgageRate,
                                          TermStructure = HorizonTermStructure,
                                          PrepaymentAssumption = prepayment,
                                          ModelTune = ModelTune,
                                          Severity = 0,
                                          Burnout = Burnout,
                                          begin.cpr = begin.cpr,
                                          end.cpr = end.cpr,
                                          seasoning.period = seasoning.period,
                                          CPR = CPR)
    
    HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                            original.bal = original.bal,
                            settlement.date = HorizonSettlement,
                            price = price,
                            PrepaymentAssumption = HorizonPrepaymentAssumption)
    
    # =========================================================================
    # This section begins the calculation of horizon total return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # ========================================================================
    
    NumberofCashFlow <- as.numeric(length(TotalCashFlow(HorizonCashFlow)))
    reinvestment.rate <- as.numeric(HorizonCurve[1,2])/yield.basis
    
    # =========================================================================
    # Horizon present value of MBS pass through using spot spread, nominal 
    # spread or OAS use switch here to compute the horizon present value based 
    # and on either spot spread, nominal spread, or horizon price. (At this 
    # time there is no OAS to price module) The functions Horizon.Spot.Value, 
    # Horizon.Nominal.Value, and Horizon.Price.Value are used to determine the
    # present value of the remaining cash flows are the horizon date.  
    # The switch function determines which function is called based on 
    # horizon.price.type
    # ========================================================================

  Horizon.Spot.Value <- function(HorizonTermStructure = "character",
                                   HorizonCashFlow = "character",
                                   HorizonSpotSpread = numeric(),
                                   NumberofCashFlow = numeric()){
  DiscountRate <- 1/((1+((SpotRate(HorizonTermStructure)[1:NumberofCashFlow] + 
                           horizon.spot.spread)/monthly.yield.basis)) ^ 
                          (Period(HorizonTermStructure)[1:NumberofCashFlow]))
      
  HorizonPresentValue <- 
    DiscountRate[1:NumberofCashFlow] * TotalCashFlow(HorizonCashFlow)
    PresentValue <- sum(HorizonPresentValue)
    return(PresentValue)}
    
  Horizon.Nominal.Value <- function(HorizonCurve = "character",
                           HorizonTermStructure = "character",
                          HorizonCashFlow = "character"){
  InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                            as.numeric(rates.data[2,2:12]),
                            data = data.frame(HorizonCurve))
      
  HorizonYield <- 
      predict(InterpolateCurve, WAL(HorizonCashFlow)) + horizon.nominal.spread
      HorizonYield <- rep(HorizonYield, NumberofCashFlow)
      DiscountRate <- 1/((1 + (HorizonYield/monthly.yield.basis))^
                           (Period(HorizonTermStructure)[1:NumberofCashFlow]))
      HorizonPresentValue <- DiscountRate * TotalCashFlow(HorizonCashFlow)
      PresentValue <- sum(HorizonPresentValue)
      return(PresentValue)}
    
  Horizon.Price.Value <- function(HorizonBond = "character",
                                    HorizonPrice = numeric()){
      original.bal * MBSFactor(HorizonBond) * (HorizonPrice/price.basis)
    }
  
  PresentValue <- switch(horizon.price.type,
    "spot" = Horizon.Spot.Value(HorizonTermStructure = HorizonTermStructure,
    HorizonCashFlow = HorizonCashFlow,
    HorizonSpotSpread = horizon.spot.spread,
    NumberofCashFlow = NumberofCashFlow),
    "nominal" = Horizon.Nominal.Value(HorizonCurve = HorizonCurve,
    HorizonTermStructure = HorizonTermStructure,
    HorizonCashFlow = HorizonCashFlow),
    "price" = Horizon.Price.Value(HorizonBond = HorizonMBS,
    HorizonPrice = horizon.price))
    
  HorizonPrice <- if(horizon.price.type == "price"){horizon.price} else {
  (PresentValue / (original.bal * MBSFactor(HorizonMBS))) * price.basis}
  
  HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                    original.bal = original.bal,
                    settlement.date = HorizonSettlement,
                    price = HorizonPrice,
                    PrepaymentAssumption = HorizonPrepaymentAssumption)

  CouponIncome <- sum(MortgageCashFlow@PassThroughInterest[1:horizon.months])

  ReceivedCashFlow <- TotalCashFlow(MortgageCashFlow)[1:horizon.months]

  n.period <- 
    as.numeric(difftime(as.Date(PmtDate(MortgageCashFlow)[horizon.months]), 
    as.Date(PmtDate(MortgageCashFlow)[1:horizon.months]), 
    units = "days")/days.in.month)
    
  TerminalValue <- 
    ReceivedCashFlow * ((1 + (reinvestment.rate/months.in.year)) ^ (n.period))
    ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow))
    
    
  PrincipalRepaid <- sum(PrepaidPrin(MortgageCashFlow)[1:horizon.months]) + 
  sum(ScheduledPrin(MortgageCashFlow)[1:horizon.months])
    
  HorizonValue <- 
    CouponIncome + 
    ReinvestmentIncome + 
    PrincipalRepaid + 
    PresentValue
  
  HorizonReturn <- (HorizonValue/proceeds)^(months.in.year/horizon.months)
  HorizonReturn <- (HorizonReturn - 1) * yield.basis
    
  new("MtgScenario",
      Period = Period(MortgageCashFlow),
      PmtDate = PmtDate(MortgageCashFlow),
      TimePeriod = TimePeriod(MortgageCashFlow),
      BeginningBal = BeginningBal(MortgageCashFlow),
      PassThroughInterest = PassThroughInterest(MortgageCashFlow),
      ScheduledPrin = ScheduledPrin(MortgageCashFlow),
      PrepaidPrin = PrepaidPrin(MortgageCashFlow),
      EndingBal = EndingBalance(MortgageCashFlow),
      TotalCashFlow = TotalCashFlow(MortgageCashFlow),
      SpotRate = SpotRate(TermStructure),
      ForwardRate = ForwardRate(TermStructure),
      SMM = SMM(Prepayment),
      YieldToMaturity = YieldToMaturity(MortgageCashFlow),
      WAL = WAL(MortgageCashFlow),
      SpreadToInterCurve = SpreadtoCurve,
      ModDuration = ModDuration(MortgageCashFlow),
      Convexity = Convexity(MortgageCashFlow), 
      EffDuration = EffDuration(MortgageTermStructure),
      EffConvexity = EffConvexity(MortgageTermStructure),
      KeyRateTenor = unname(KeyRateTenor(MortgageTermStructure)),
      KeyRateDuration = unname(KeyRateDuration(MortgageTermStructure)),
      KeyRateConvexity = unname(KeyRateConvexity(MortgageTermStructure)),
      HorizonReturn = HorizonReturn,
      HorizonMos = horizon.months,
      Name = Name(Scenario),
      Type = Type(Scenario),
      Horizon = Horizon(Scenario),
      ShiftType = ShiftType(Scenario),
      Shiftbps = Shiftbps(Scenario),
      Formula = ScenarioFormula(Scenario))
  }
  
  