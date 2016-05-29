  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2014  Glenn M Schultz, CFA



  #' An S4 Class A list of Scenario outcomes
  #'
  #' The class Mtg.ScenarioSet is a list of classes Scenario
  #' @slot Scenario A list of the class Scenario
  #' @exportClass Mtg.ScenarioSet
  setClass("Mtg.ScenarioSet",
         representation(
           Scenario = "list"))

  #' An S4 Class representing scenario analysis details
  #' 
  #' The class Scenario is the representation of the 
  #' scenario run by the investor
  #' @slot Name A character the name of the scenario
  #' @slot Type A character the type of the scenario
  #' @slot Horizon A character the time horizon over which the 
  #' scenario is run
  #' @slot ShiftType A character the interest rate shift type (e.g.
  #' twist, parallel, bull flatten, or bull steepen)
  #' @slot Shiftbps A numeric value the scenario shift in basis points
  #' @slot Formula A function represnting the shift which applied to the term structure
  #' @exportClass Scenario
  setClass("Scenario",
         representation(
           Name = "character",
           Type = "character",
           Horizon = "character",
           ShiftType = "character",
           Shiftbps = "numeric",
           Formula = "function"
         ))
  
  #' An S4 Class representing the results of mortgage return scenario analysis
  #' 
  #' The class Mtg. Scenario holds the results of a scenario analysis run
  #' @slot Period A numeric value the period number forward from current
  #' @slot PmtDate A character the payment date on which the cashflow is expected to be received
  #' @slot TimePeriod A numeric value the period stated as a unit of time
  #' @slot BeginningBal A numeric value the forecasted beginning balance in the period
  #' @slot PassThroughInterest A numeric value the interest paid through to the investor
  #' @slot ScheduledPrin A numeric value the scheduled principal due in the period
  #' @slot PrepaidPrin A numeric value the forecasted prepaid principal in the period
  #' @slot EndingBal A numeric value the forecasted ending balance in the period
  #' @slot TotalCashFlow A numeric value the forecasted total cashflow received by
  #' the investor in the period
  #' @slot spotrate A numeric vector the spot rate curve
  #' @slot forwardrate A numeric vector the forward rate curve
  #' @slot SMM A numeric vector the forecasted SMM vector
  #' @slot YieldToMaturity A numeric value the scenario yield to maturity
  #' @slot WAL A numeric value the scenario weighted average life
  #' @slot SpreadToInterCurve A numeric value the spread to the interpolated curve
  #' @slot ModDuration A numeric value the scenario modified duration
  #' @slot Convexity A numeric value the scenario convexity
  #' @slot EffDuration A numeric value the effective duration which is the sum of the
  #' key rate durations
  #' @slot EffConvexity A numeric value the effective duration which is teh sum of the
  #' key rate convexities
  #' @slot KeyRateTenor A numeric vector the key rate tenors
  #' @slot KeyRateDuration A numeric vector the duration of each key rate tenor
  #' @slot KeyRateConvexity A numeric vector the convexity of each each key rate tenor
  #' @slot HorizonReturn A numeric value the horizon total return
  #' @exportClass Mtg.Scenario     
  setClass("Mtg.Scenario",
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
           spotrate = "numeric",
           forwardrate = "numeric",
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
           HorizonReturn = "numeric"),
         contains = "Scenario")

  setMethod("initialize",
          signature("Mtg.ScenarioSet"),
          function(.Object,
                   Scenario = "list")
            {.Object@Scenario = Scenario
            
             return(.Object)
             })
  
  setMethod("initialize",
            signature("Scenario"),
            function(.Object,
                    Name = "character",
                    Type = "character",
                    Horizon = "character",
                    ShiftType = "character",
                    Shiftbps = "numeric",
                    Formula = "function")
            {.Object@Name = Name
             .Object@Type = Type
             .Object@Horizon = Horizon
             .Object@ShiftType
             .Object@Shiftbps
             .Object@Formula
             
             return(.Object)

            })

  setMethod("initialize",
          signature("Mtg.Scenario"),
          function(.Object,
          Period = "numeric",
          PmtDate = "character",
          TimePeriod = "numeric",
          BeginningBal = "numeric",
          PassThroughInterest = "numeric",
          ScheduledPrin = "numeric",
          PrepaidPrin = "numeric",
          EndingBal = "numeric",
          TotalCashFlow = "numeric",
          spotrate = "numeric",
          forwardrate = "numeric",
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
          Name = "character",
          Type = "character",
          Horizon = "character",
          ShiftType = "character",
          Shiftbps = "numeric",
          Formula = "function")
          {
            .Object@Period = Period
            .Object@PmtDate = PmtDate
            .Object@TimePeriod = TimePeriod
            .Object@BeginningBal = BeginningBal
            .Object@PassThroughInterest = PassThroughInterest
            .Object@ScheduledPrin = ScheduledPrin
            .Object@PrepaidPrin = PrepaidPrin
            .Object@EndingBal = EndingBal
            .Object@TotalCashFlow = TotalCashFlow
            .Object@spotrate = spotrate
            .Object@forwardrate = forwardrate
            .Object@SMM = SMM
            .Object@YieldToMaturity = YieldToMaturity
            .Object@WAL = WAL
            .Object@SpreadToInterCurve = SpreadToInterCurve
            .Object@ModDuration = ModDuration
            .Object@Convexity = Convexity 
            .Object@EffDuration = EffDuration
            .Object@EffConvexity = EffConvexity
            .Object@KeyRateTenor = KeyRateTenor
            .Object@KeyRateDuration = KeyRateDuration
            .Object@KeyRateConvexity = KeyRateConvexity
            .Object@HorizonReturn = HorizonReturn
            .Object@Name = Name
            .Object@Type = Type
            .Object@Horizon = Horizon
            .Object@ShiftType = ShiftType
            .Object@Shiftbps = Shiftbps
            .Object@Formula = Formula
            
            return(.Object)
  
          })

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
  #' @export Mtg.Scenario
  Mtg.Scenario <- function(bond.id ="character",
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
  
  # =================================================================================================
  # Mortgage Scenario analysis is done in two steps
  # The first is calculated the expected cash-flows received over the investment horizon
  # The second is to "roll" the pass through security forward and price the expected future cash-flows
  # =================================================================================================
  
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
  Burnout = bond.id@Burnout
  Scenario <- ScenarioCall(Scenario = scenario)
  rates <- rates.data
  
  rates[1,2:length(rates)] <- Scenario@Formula(rates[1,1:length(rates)], Shiftbps = Scenario@Shiftbps)
  
  TermStructure <- TermStructure(rates.data = rates, 
                                 method = method)
  
  
  Prepayment <- PrepaymentAssumption(bond.id = bond.id, 
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
  
  InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ as.numeric(rates.data[2,2:12]), 
                            data = data.frame(rates.data))  
  
  SpreadtoCurve <- (MortgageCashFlow@YieldToMaturity * yield.basis) - predict(InterpolateCurve, MortgageCashFlow@WAL)
  
  proceeds <- MortgageCashFlow@Accrued + (original.bal * bond.id@MBSFactor * (price/price.basis))
  principal <- original.bal * bond.id@MBSFactor
  
  MortgageTermStructure <- MtgTermStructure(bond.id = bond.id, 
                                            original.bal = original.bal, 
                                            Rate.Delta = rate.delta, 
                                            TermStructure = TermStructure,
                                            settlement.date = settlement.date, 
                                            principal = principal, 
                                            price = price, 
                                            cashflow = MortgageCashFlow)
  
  # =================================================================================================
  # Begin horizon mortgage pass-through analysis
  # =================================================================================================
  
  HorizonCurve <- rates
  HorizonCurve[1,1] <- as.character(as.Date(HorizonCurve[1,1]) %m+% months(horizon.months))
  HorizonSettlement <- as.Date(settlement.date, format = "%m-%d-%Y") %m+% months(horizon.months)
  
  
  HorizonTermStructure <- TermStructure(rates.data = HorizonCurve,
                                        method = "ns")
  
  
  ForwardPassThrough(bond.id = bond.id,
                     original.bal = original.bal,
                     projected.cashflow = MortgageCashFlow,
                     horizon.months = horizon.months)
  
  HorizonConn <- gzfile(paste(system.file(package = "BondLab"),
                              "/Temp_BondData/","TempPassThrough.rds", sep = ""))
  
  HorizonMBS <- readRDS(HorizonConn)
  
  HorizonPrepaymentAssumption <- PrepaymentAssumption(bond.id = HorizonMBS,
                                                      TermStructure = HorizonTermStructure,
                                                      MortgageRate = MortgageRate,
                                                      ModelTune = ModelTune,
                                                      Burnout = Burnout,
                                                      PrepaymentAssumption = prepayment,
                                                      Severity = 0)
  
  HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                                      original.bal = original.bal,
                                      settlement.date = HorizonSettlement,
                                      price = price,
                                      PrepaymentAssumption = HorizonPrepaymentAssumption)
  
  # =====================================================================================================
  # This section begins the calculation of horizon total return
  # Cashflow Received + Reinvestment Income + Present Value at Horizon
  # =====================================================================================================
  
  NumberofCashFlow <- as.numeric(length(HorizonCashFlow@TotalCashFlow))
  reinvestment.rate <- as.numeric(HorizonCurve[1,2])/yield.basis
  
  # ====================================================================================================
  # Horizon present value of MBS pass through using spot spread, nominal spread or OAS
  # use switch here to compute the horizon present value based and on either spot spread, nominal spread, 
  # or horizon price. (At this time there is no OAS to price module)
  # The functions Horizon.Spot.Value, Horizon.Nominal.Value, and Horizon.Price.Value are used to determine the
  # present value of the remaining cash flows are the horizon date.  The switch function determines which
  # function is called based on horizon.price.type
  # ====================================================================================================
  
  Horizon.Spot.Value <- function(HorizonTermStructure = "character",
                                 HorizonCashFlow = "character",
                                 HorizonSpotSpread = numeric(),
                                 NumberofCashFlow = numeric()){
    DiscountRate <- 1/((1+((HorizonTermStructure@SpotRate[1:NumberofCashFlow] + 
                              horizon.spot.spread)/monthly.yield.basis)) ^ 
                         (HorizonTermStructure@Period[1:NumberofCashFlow]))
    
    HorizonPresentValue <- DiscountRate[1:NumberofCashFlow] * HorizonCashFlow@TotalCashFlow
    PresentValue <- sum(HorizonPresentValue)
    return(PresentValue)}
  
  Horizon.Nominal.Value <- function(HorizonCurve = "character",
                                    HorizonTermStructure = "character",
                                    HorizonCashFlow = "character"){
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                                as.numeric(rates.data[2,2:12]),
                              data = data.frame(HorizonCurve))
    
    HorizonYield <- predict(InterpolateCurve, HorizonCashFlow@WAL) + horizon.nominal.spread
    HorizonYield <- rep(HorizonYield, NumberofCashFlow)
    DiscountRate <- 1/((1 + (HorizonYield/monthly.yield.basis))^
                         (HorizonTermStructure@Period[1:NumberofCashFlow]))
    HorizonPresentValue <- DiscountRate * HorizonCashFlow@TotalCashFlow
    PresentValue <- sum(HorizonPresentValue)
    return(PresentValue)}
  
  Horizon.Price.Value <- function(HorizonBond = "character",
                                  HorizonPrice = numeric()){
    original.bal * HorizonBond@MBSFactor * (HorizonPrice/price.basis)
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
                                                       HorizonPrice = horizon.price)
  )
  
  HorizonPrice <- if(horizon.price.type == "price"){horizon.price} else {
    (PresentValue / (original.bal * HorizonMBS@MBSFactor)) * price.basis}
  
  HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                                      original.bal = original.bal,
                                      settlement.date = HorizonSettlement,
                                      price = HorizonPrice,
                                      PrepaymentAssumption = HorizonPrepaymentAssumption)
  
  # ====================================================================================================
  # Coupon Income
  # ====================================================================================================
  
  CouponIncome <- sum(MortgageCashFlow@PassThroughInterest[1:horizon.months])
  
  # ====================================================================================================
  # Reinvestment Income
  # ====================================================================================================
  ReceivedCashFlow <- MortgageCashFlow@TotalCashFlow[1:horizon.months]
  
  n.period <- as.numeric(difftime(as.Date(MortgageCashFlow@PmtDate[horizon.months]), 
                                  as.Date(MortgageCashFlow@PmtDate[1:horizon.months]), units = "days")/days.in.month)
  
  TerminalValue <- ReceivedCashFlow * ((1 + (reinvestment.rate/months.in.year)) ^ (n.period))
  ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow))
  
  # ==================================================================================================
  # Principal Returned
  # ==================================================================================================
  
  PrincipalRepaid <- sum(MortgageCashFlow@PrepaidPrin[1:horizon.months]) + 
    sum(MortgageCashFlow@ScheduledPrin[1:horizon.months])
  
  
  
  HorizonValue <- CouponIncome + ReinvestmentIncome + PrincipalRepaid + PresentValue
  HorizonReturn <- (HorizonValue/proceeds)^(months.in.year/horizon.months)
  HorizonReturn <- (HorizonReturn - 1) * yield.basis
  
              new("Mtg.Scenario",  
              Period = MortgageCashFlow@Period,
              PmtDate = MortgageCashFlow@PmtDate,
              TimePeriod = MortgageCashFlow@TimePeriod,
              BeginningBal = MortgageCashFlow@BeginningBal,
              PassThroughInterest = MortgageCashFlow@PassThroughInterest,
              ScheduledPrin = MortgageCashFlow@ScheduledPrin,
              PrepaidPrin = MortgageCashFlow@PrepaidPrin,
              EndingBal = MortgageCashFlow@EndingBal,
              TotalCashFlow = (MortgageCashFlow@PassThroughInterest + 
                                 MortgageCashFlow@ScheduledPrin + 
                                 MortgageCashFlow@PrepaidPrin),
              spotrate = TermStructure@SpotRate,
              forwardrate = TermStructure@ForwardRate,
              SMM = Prepayment@SMM,
              YieldToMaturity = MortgageCashFlow@YieldToMaturity,
              WAL = MortgageCashFlow@WAL,
              SpreadToInterCurve = SpreadtoCurve,
              ModDuration = MortgageCashFlow@ModDuration,
              Convexity = MortgageCashFlow@Convexity,
              EffDuration = MortgageTermStructure@EffDuration,
              EffConvexity =  MortgageTermStructure@EffConvexity,
              KeyRateTenor =  MortgageTermStructure@KeyRateTenor,
              KeyRateDuration =  MortgageTermStructure@KeyRateDuration,
              KeyRateConvexity =  MortgageTermStructure@KeyRateConvexity,
              HorizonReturn = HorizonReturn,
              Name = Scenario@Name,
              Type = Scenario@Type,
              Horizon = Scenario@Horizon,
              ShiftType = Scenario@ShiftType,
              Shiftbps = Scenario@Shiftbps,
              Formula = Scenario@Formula)}
