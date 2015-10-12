  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Glenn M Schultz, CFA

  setMethod("initialize",
          signature("Mtg.ScenarioSet"),
          function(.Object,
                   Scenario = "list")
            {.Object@Scenario = Scenario
             return(.Object)
             callNextMethod("Mtg.ScenarioSet")
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
             callNextMethod(.Object,...)
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
            callNextMethod(.Object,...)
  
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
  #' @param horizon.spread A numeric value the horizon spread
  #' @param method A character string the method used to fit the term structure
  #' @param prepayment A character string the prepayment assumption
  #' @param ... Optional values when PSA or CPR is used
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
                           horizon.spread = numeric(),
                           method = "character",
                           prepayment = "character",
                           ...,
                           begin.cpr = numeric(),
                           end.cpr = numeric(),
                           seasoning.period = numeric(),
                           CPR = numeric()) { 
  
  # =================================================================================================
  # Mortgage Scenario analysis is done in two steps
  # The first is calculated the expected cash-flows received over the investment horizon
  # The second is to "roll" the pass through security forward and price the expected future cash-flows
  # =================================================================================================
  
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
                                                        PrepaymentAssumption = prepayment)
    
    HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                                        original.bal = original.bal,
                                        settlement.date = HorizonSettlement,
                                        price = price,
                                        PrepaymentAssumption = HorizonPrepaymentAssumption)
    
    # =====================================================================================================
    # Calculate Total Return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # =====================================================================================================
    
    NumberofCashFlow <- as.numeric(length(HorizonCashFlow@TotalCashFlow))
    reinvestment.rate <- as.numeric(HorizonCurve[1,2])/yield.basis
    
    DiscountRate <-  1/((1+((HorizonTermStructure@spotrate[1:NumberofCashFlow] + horizon.spread)/1200)) ^ 
                          (HorizonTermStructure@period[1:NumberofCashFlow]))
    
    HorizonPresentValue <- DiscountRate[1:NumberofCashFlow] * HorizonCashFlow@TotalCashFlow
    
    
    ReceivedCF <- MortgageCashFlow@TotalCashFlow[1:horizon.months]
    
    n.period <- as.numeric(difftime(as.Date(MortgageCashFlow@PmtDate[horizon.months]), 
                                    as.Date(MortgageCashFlow@PmtDate[1:horizon.months]), units = "days")/days.in.month)
    
    
    Reinvestment <- ReceivedCF * ((1 + (reinvestment.rate/months.in.year)) ^ (n.period))
    
    HorizonValue <- sum(HorizonPresentValue) + sum(Reinvestment)
    HorizonReturn <- HorizonValue/proceeds
    HorizonReturn <- (HorizonReturn - 1) * yield.basis
    
    temp <- new("Mtg.Scenario",  
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
                    spotrate = TermStructure@spotrate,
                    forwardrate = TermStructure@forwardrate,
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
                    Formula = Scenario@Formula)                     

  }
  