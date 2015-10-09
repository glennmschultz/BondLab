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
  #' @param rates.data A character string the trade date mm-dd-yyyy
  #' @param price A character string the trade date mm-dd-yyyy
  #' @param original.bal A numeric value the price
  #' @param scenario A character string the scenario
  #' @param method A character string the method used to fit the term structure
  Mtg.Scenario <- function(bond.id ="character",
                           rates.data = "character",
                           price = numeric(), 
                           original.bal = numeric(),
                           scenario = "character",
                           method = "character") { 
  

  bond.id <- bond.id

  # First step open mortgage rate functions
  MortgageRate <- MtgRate()
  
  ModelTune <- ModelTune(bond.id = bond.id)
  Burnout = bond.id@Burnout
  
  Scenario <- ScenarioCall(Scenario = scenario)

  # Third call the trade date rates data
  rates <- rates.data
    
  # Fourth call the scenario 
  rates[1,2:length(rates)] <- Scenario@Formula(rates[1,1:length(rates)], Shiftbps = Scenario@Shiftbps)
    
  # Caclulate the term strucute
  TermStructure <- TermStructure(rates.data = rates, 
                                 method = method)
    
  # Run the prepayment model
  Prepayment <- PrepaymentAssumption(bond.id = bond.id, 
                                    MortgageRate = MortgageRate, 
                                    TermStructure = TermStructure, 
                                    PrepaymentAssumption = PrepaymentAssumption, 
                                    ModelTune = ModelTune, 
                                    Burnout = Burnout, 
                                    begin.cpr = begin.cpr, 
                                    end.cpr = end.cpr, 
                                    seasoning.period = seasoning.period, 
                                    CPR = CPR)
    
  # Scenario Mortgage cash flow analysis 
  MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                      original.bal = original.bal, 
                                      settlement.date = settlement.date, 
                                      price = price, 
                                      PrepaymentAssumption = Prepayment)
    
  # Calculate static cash flow spread to the curve                                      
  InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ as.numeric(rates.data[2,2:12]), 
                            data = data.frame(rates.data))  
    
  SpreadtoCurve <- (MortgageCashFlow@YieldToMaturity * yield.basis) - predict(InterpolateCurve, MortgageCashFlow@WAL)
    
  # The second step will be to calculate the scenario effective duration and convexity
  MortgageTermStructure <- MtgTermStructure(bond.id = bond.id, 
                                            original.bal = original.bal, 
                                            Rate.Delta = 0.25, 
                                            TermStructure = TermStructure,
                                            settlement.date = 
                                            settlement.date, 
                                            principal = original.bal * bond.id@MBSFactor, 
                                            price = price, 
                                            cashflow = MortgageCashFlow)
    
    ForwardPassThrough(bond.id = bond.id,
                      original.bal = original.bal,
                      ProjectedCashFlow = MortgageCashFlow,
                      HorizonMonths = HorizonMonths)
    
    
    HorizonReturn <- ReturnAnalysis(Scenario = MortgageCashFlow, 
                                    proceeds = proceeds,
                                    settlement.date = settlement.date, 
                                    spot.spread = MortgageTermStructure@SpotSpread, 
                                    HorizonMonths = 12, 
                                    ReinvestmentRate = .0025)
    
    HorizonReturn = (HorizonReturn - 1) * 100
    
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
    
    ScenarioResult <- append(ScenarioResult, temp)
    
  
  } # scenario end function
  
  # =====================================================================================
  # Function to compute the horizon return 
  # =====================================================================================
  
  #' Function to compute the horizon return
  #' 
  #' A function to compute the horizon return of a PassThrough
  #' @param Scenario a character string referring to scenario mortgage cashflow
  #' @param settlement.date a character string the settlment date
  #' @param proceeds a numeric value the investor initial outlay
  #' @param spot.spread a numeric value the spread to the spot curve
  #' @param HorizonMonths a numeric value the investment horizon in months
  #' @param ReinvestmentRate a numeric value the reinvestment assumption 
  ReturnAnalysis <- function(Scenario = "character", 
                             settlement.date = "character", 
                             proceeds = numeric(), 
                             spot.spread = numeric(), 
                             HorizonMonths = numeric(), 
                             ReinvestmentRate = numeric()) {
    
    ReceivedCF <- Scenario@TotalCashFlow[1:HorizonMonths]
    n.period <- as.numeric(difftime(as.Date(Scenario@PmtDate[HorizonMonths]), 
                                    as.Date(Scenario@PmtDate[1:HorizonMonths]), units = "days")/days.in.month)
    
    
    Reinvestment <- ReceivedCF * (1 + (ReinvestmentRate/months.in.year)) ^ (n.period)
    
    Reinvestment <- sum(Reinvestment)
    
    # ------------------------------------------------------------------------------------------
    # this is code which is to be factored out
    # Price the tail cash flow priced at horizon
    # Forward month indexes to the cashflow array
    FwdMonth <- (HorizonMonths + 1)
    FwdSettleDate <- as.Date(settlement.date, "%m-%d-%Y")  %m+% months(HorizonMonths)
    
    FwdCashFlowPmtDate <- Scenario@PmtDate[FwdMonth:length(Scenario@PmtDate)]
    
    RemainingCF <- Scenario@TotalCashFlow[FwdMonth:length(Scenario@TotalCashFlow)]
    
    n.period.fwd <- as.numeric(difftime(as.Date(Scenario@PmtDate[FwdMonth:length(Scenario@PmtDate)]), 
                                        as.Date(FwdSettleDate), units = "days")/days.in.month)
    
    # The approach bases the investor return on the forward rate curve and forward rates.
    DiscountRate <-  
      # Spot rates at horizon to length of cashflows
      (1+((TermStructure@spotrate[FwdMonth:length(Scenario@TotalCashFlow)] + spot.spread)/1200))  ^
      # Index time period to period less length of the horizon for discounting the forward price
      (-1 * n.period.fwd)
    
    DiscPV <- sum(RemainingCF * DiscountRate)
    TotalHrzProceeds <- Reinvestment + DiscPV
    
    Return <- TotalHrzProceeds/proceeds 
    # -------------------------------------------------------------------------------------------
  }



