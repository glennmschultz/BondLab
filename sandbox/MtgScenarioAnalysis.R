  setClass("Mtg.ScenarioSet",
           representation(
             Scenario = "list"))

  setClass("Scenario",
          representation(
             Name = "character",
             Type = "character",
             Horizon = "character",
             ShiftType = "character",
             Shiftbps = "numeric",
             Formula = "function"))

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
           SpreadToCurve = "numeric",
           ModDuration = "numeric",
           Convexity = "numeric", 
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric",
           HorizonReturn = "numeric"))
  
  
#  setGeneric("Mtg.Scenario", 
#  function(bond.id ="character", trade.date = "character", settlement.date = "character", price = numeric(), proceeds = numeric(),
#  spot.spread = numeric(), original.bal = numeric(), scenario.set = vector(), rates.data = "character", 
#  method = "character", PrepaymentAssumption = "character",..., ModelTune = "character", Burnout = numeric(),
#  begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
#  {standardGeneric("Mtg.Scenario")})
  
  
  setMethod("initialize", 
            signature("Mtg.ScenarioSet"),
            function(.Object,...,
                     Scenario = list())
            {
              .Object@Scenario = Scenario
              return(.Object)
              callNextMethod(.Object,...)
            })

  setMethod("initialize",
           signature("Scenario"),
           function(.Object,
                    Name = "character",
                    Type = "character",
                    Horizon = "character",
                    ShiftType = "character",
                    Shiftbps = numeric(),
                    Formula = "function")
           {
             .Object@Name = Name
             .Object@Type = Type
             .Object@Horizon = Horizon
             .Object@ShiftType = ShiftType
             .Object@Shiftbps = Shiftbps
             .Object@Formula = Formula
             
             
             return(.Object)
             callNextMethod(.Object,...)
           })

 
   setMethod("initialize",
             signature("Mtg.Scenario"),
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
                      spotrate = numeric(),
                      forwardrate = numeric(),
                      SMM = numeric(),
                      YieldToMaturity = numeric(),
                      WAL = numeric(),
                      SpreadToCurve = numeric(),
                      ModDuration = numeric(),
                      Convexity = numeric(), 
                      EffDuration = numeric(),
                      EffConvexity = numeric(),
                      KeyRateTenor = numeric(),
                      KeyRateDuration = numeric(),
                      KeyRateConvexity = numeric(),
                      HorizonReturn = numeric(),
                      Name = "character",
                      Type = "character",
                      Horizon = "character",
                      ShiftType = "character",
                      Shiftbps = numeric(),
                      Formula = Formula)
                      
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
                        .Object@YieldToMaturity = YieldtoMaturity
                        .Object@WAL = WAL
                        .Object@SpreadToCurve = SpreadToCurve
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
                        .Objec@ShiftType = ShiftType
                        .Object@Shiftbps = Shiftbps
                        .Object@Formula = Formula
                       
                        return(.Object)
                        callNextMethod(.Object,...)
                       })
  
# ===================== Scenario Function ===================


    Mtg.Scenario <- function(bond.id ="character", trade.date = "character", settlement.date = "character", price = numeric(), proceeds = numeric(), 
                         spot.spread = numeric(), original.bal = numeric(), scenario.set = vector(), rates.data = "character", 
                         method = "character", PrepaymentAssumption = "character",..., ModelTune = "character", Burnout = numeric(),
                         begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()) { 
  
    if(missing(method)) method = "ns"
  
    ScenarioResult <- list()
  
    # First step open mortgage rate functions
    connS1 <- gzfile(description = "~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
    MortgageRate <- readRDS(connS1)
  
    #Call the desired curve from rates data folder
    trade.date = as.Date(trade.date, "%m-%d-%Y")
  
    connS2 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
    rates.data <- readRDS(connS2)
  
    #Call Prepayment Model
    connS3 <- gzfile(description = paste("~/BondLab/PrepaymentModel/",bond.id@Model,".rds", sep = ""), open = "rb")
    ModelTune <- readRDS(connS3)
    Burnout = bond.id@Burnout
    
    # ================== Scenario Analysis ========================
    # open connection to scenario library
    for(i in 1:length(scenario.set)){
    
    connS4 <- gzfile(description = paste("~/BondLab/Scenario/", as.character(scenario.set[i]), ".rds", sep =""), open = "rb")        
    Scenario <- readRDS(connS4) 
    
    # Third call the trade date rates data
    # Create variabel for the length of rates and rates data 
    rates <- rates.data
    
    # Fourth call the scenario 
    rates[1,2:length(rates)] <- Scenario@Formula(rates[1,1:length(rates)], Shiftbps = Scenario@Shiftbps)
    
    # Caclulate the term strucute
    TermStructure = TermStructure(rates.data = rates, method = method)
    
    # Run the prpeayment model
    Prepayment = PrepaymentAssumption(bond.id = bond.id, MortgageRate = MortgageRate, TermStructure = TermStructure, 
                                      PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                                      begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
    
    # Scenario Mortgage cash flow analysis 
    MortgageCashFlow = MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, 
                                         settlement.date = settlement.date, price = price, PrepaymentAssumption = Prepayment)
    
    # Calculate static cash flow spread to the curve                                      
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ as.numeric(rates.data[2,2:12]), data = data.frame(rates.data))  
    SpreadtoCurve = (MortgageCashFlow@YieldToMaturity * 100) - predict(InterpolateCurve, MortgageCashFlow@WAL )
    
    # The second step will be to calculate the scenario effective duration and convexity
    # I can do this with a different call that does not require MtgTermStrucutre
    MortgageTermStructure = MtgTermStructure(bond.id = bond.id, original.bal = original.bal, Rate.Delta = 0.25, TermStructure = TermStructure,
                                             settlement.date = settlement.date, principal = original.bal * MortgageCashFlow@MBSFactor, price = price, cashflow = MortgageCashFlow)
    
    # ================= Return Analysis Function ====================
    # ============== This is an encapasulated function ==============    
    # =============Function to compute the horizon return ===========
    
    ReturnAnalysis <- function(Scenario = "character", settlement.date = "character", proceeds = numeric(), 
                               MortgageTermStructure = "character", spot.spread = numeric(), HrzMonths = numeric(), 
                               ReinvestmentRate = numeric()) {
      
      # Need to error trap the reinvestment rate     
      # Reinvestment of cash flow  
      
      ReceivedCF = Scenario@TotalCashFlow[1:HrzMonths]
      n.period = as.numeric(difftime(as.Date(Scenario@PmtDate[HrzMonths]), as.Date(Scenario@PmtDate[1:HrzMonths]), units = "days")/days.in.month)
      Reinvestment = ReceivedCF * (1 + (ReinvestmentRate/months.in.year)) ^ (n.period)
      Reinvestment = sum(Reinvestment)
      
      # Price the tail cash flow priced at horizon
      # Forward month indexes to the cashflow array
      FwdMonth = (HrzMonths + 1)
      FwdSettleDate = as.Date(settlement.date, "%m-%d-%Y")  %m+% months(HrzMonths)
      FwdCashFlowPmtDate = Scenario@PmtDate[FwdMonth:length(Scenario@PmtDate)]
      RemainingCF = Scenario@TotalCashFlow[FwdMonth:length(Scenario@TotalCashFlow)]
      n.period.fwd = as.numeric(difftime(as.Date(Scenario@PmtDate[FwdMonth:length(Scenario@PmtDate)]), as.Date(FwdSettleDate), units = "days")/days.in.month)
      
      # The approach bases the investor return on the forward rate curve and forward rates.
      DiscountRate =  
        # Spot rates at horizon to length of cashflows
        (1+((TermStructure@spotrate[FwdMonth:length(Scenario@TotalCashFlow)] + spot.spread)/1200))  ^
        # Index time period to period less length of the horizon for discounting the forward price
        (-1 * n.period.fwd)
      
      DiscPV = sum(RemainingCF * DiscountRate)
      TotalHrzProceeds = Reinvestment + DiscPV
      
      Return = TotalHrzProceeds/proceeds 
      
    }
    # ============== End of Mortgage Return Function =====================
    
    HorizonReturn <- ReturnAnalysis(Scenario = MortgageCashFlow, MortgageTermStructure = MortgageTermStructure, proceeds = proceeds,
                                    settlement.date = settlement.date, spot.spread = spot.spread, HrzMonths = 12, ReinvestmentRate = .0025)
    
    HorizonReturn = (HorizonReturn - 1) * 100
    
    Scenario <- new("Mtg.Scenario",  
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
                    SpreadToCurve = SpreadtoCurve,
                    ModDuration = MortgageCashFlow@ModDuration,
                    Convexity = MortgageCashFlow@Convexity,
                    EffDuration = MortgageTermStructure@EffDuration,
                    EffConvexity =  MortgageTermStructure@EffConvexity,
                    KeyRateTenor =  MortgageTermStructure@KeyRateTenor,
                    KeyRateDuration =  MortgageTermStructure@KeyRateDuration,
                    KeyRateConvexity =  MortgageTermStructure@KeyRateConvexity,
                    HorizonReturn = HorizonReturn,
                    Scenario                      
    )
    
    ScenarioResult <- append(ScenarioResult, Scenario)
    
   } # end loop
  
  
  new("Mtg.ScenarioSet", 
      Scenario = ScenarioResult)
   
  
  }

  # ==================== The Scenario Function End Here ===================

  
  # ============== build contructor for Scenario
 
