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
          signature("PassThroughAnalytics"),
          function(.Object,
                   Cusip = "character",
                   ID = "character",
                   BondType = "character",
                   Sector ="character",
                   Coupon = "numeric",
                   IssueDate = "character",
                   DatedDate = "character",
                   Maturity = "character",
                   LastPmtDate = "character",
                   NextPmtDate = "character",
                   PaymentDelay = "numeric",
                   Moody = "character",
                   SP = "character",
                   BondLab  = "character",
                   Frequency = "numeric",
                   BondBasis = "character",
                   GWac = "numeric",
                   AmortizationType = "character",
                   AmortizationTerm = "numeric",
                   Index = "character",
                   Margin = "numeric",
                   FirstPmtDate = "character",
                   FinalPmtDate = "character",
                   Servicing = "numeric",
                   PMI = "numeric",
                   Gfee = "numeric",
                   InitialInterest = "character",
                   InterestOnlyPeriod = "numeric",
                   FirstPrinPaymentDate = "character",
                   BalloonPmt = "character",
                   BalloonDate = "character",
                   MBSFactor = "numeric",
                   Model = "character",
                   Burnout = "numeric",
                   SATO = "numeric",
                   #--- mortgage cashflow slots
                   Price = "numeric",
                   Accrued = "numeric",
                   YieldToMaturity = "numeric",
                   WAL = "numeric",
                   ModDuration = "numeric",
                   Convexity = "numeric",
                   Period = "numeric",
                   PmtDate = "character",
                   TimePeriod = "numeric",
                   BeginningBal = "numeric",
                   MonthlyPmt = "numeric",
                   MonthlyInterest = "numeric",
                   PassThroughInterest = "numeric",
                   ScheduledPrin = "numeric",
                   PrepaidPrin = "numeric",
                   EndingBal = "numeric",
                   ServicingIncome = "numeric",
                   PMIPremium = "numeric",
                   GFeePremium = "numeric",  
                   TotalCashFlow = "numeric",
                   #---- mortgage term structure slots
                   SpotSpread = "numeric",   
                   EffDuration = "numeric",
                   EffConvexity = "numeric",
                   KeyRateTenor = "numeric",
                   KeyRateDuration = "numeric",
                   KeyRateConvexity = "numeric",
                   #---- term structure
                   tradedate = "character",
                   period = "numeric",
                   date = "character",
                   spotrate = "numeric",
                   forwardrate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric",
                   #--- prepayment assumption 
                   PrepayAssumption = "character",
                   PPCStart = "numeric",
                   PPCEnd = "numeric",
                   PPCSeasoning = "numeric",
                   #FirstPmtDate = "character",
                   #LastPmtDate = "character",
                   #FinalPmtDate = "character",
                   #PmtDate = "character",
                   LoanAge = "numeric",
                   #Period = "numeric",
                   NoteRate = "numeric",
                   MtgRateFwd = "numeric",
                   Incentive = "numeric",
                   SMM = "numeric",
                   MDR = "numeric",
                   #--- Mtg Scenario Set
                   Scenario = "list"){
            
            .Object@Cusip = Cusip
            .Object@ID = ID
            .Object@BondType = BondType
            .Object@Sector = Sector
            .Object@Coupon = Coupon
            .Object@IssueDate = IssueDate
            .Object@DatedDate = DatedDate
            .Object@Maturity = Maturity
            .Object@LastPmtDate = LastPmtDate
            .Object@NextPmtDate = NextPmtDate
            .Object@PaymentDelay = PaymentDelay
            .Object@Moody = Moody
            .Object@SP = SP
            .Object@BondLab  = BondLab
            .Object@Frequency = Frequency
            .Object@BondBasis = BondBasis
            .Object@GWac = GWac
            .Object@AmortizationType = AmortizationType
            .Object@AmortizationTerm = AmortizationTerm
            .Object@Index = Index
            .Object@Margin = Margin
            .Object@FirstPmtDate = FirstPmtDate
            .Object@FinalPmtDate = FinalPmtDate
            .Object@Servicing = Servicing
            .Object@PMI = PMI
            .Object@Gfee = Gfee
            .Object@InitialInterest = InitialInterest
            .Object@InterestOnlyPeriod = InterestOnlyPeriod
            .Object@FirstPrinPaymentDate = FirstPrinPaymentDate
            .Object@BalloonPmt = BalloonPmt
            .Object@BalloonDate = BalloonDate
            .Object@MBSFactor = MBSFactor
            .Object@Model = Model
            .Object@Burnout = Burnout
            .Object@SATO = SATO
            #---- mortgage cashflow slots
            .Object@Price = Price
            .Object@Accrued = Accrued
            .Object@YieldToMaturity = YieldToMaturity
            .Object@WAL = WAL
            .Object@ModDuration = ModDuration
            .Object@Convexity = Convexity
            .Object@Period = Period
            .Object@PmtDate = PmtDate
            .Object@TimePeriod = TimePeriod
            .Object@BeginningBal = BeginningBal
            .Object@MonthlyPmt = MonthlyPmt
            .Object@MonthlyInterest = MonthlyInterest
            .Object@PassThroughInterest = PassThroughInterest
            .Object@ScheduledPrin = ScheduledPrin
            .Object@PrepaidPrin = PrepaidPrin
            .Object@EndingBal = EndingBal
            .Object@ServicingIncome = ServicingIncome
            .Object@PMIPremium = PMIPremium
            .Object@GFeePremium = GFeePremium
            .Object@TotalCashFlow = TotalCashFlow
            #------ mortgage term structure slots
            .Object@SpotSpread = SpotSpread
            .Object@EffDuration = EffDuration
            .Object@EffConvexity = EffConvexity
            .Object@KeyRateTenor = KeyRateTenor
            .Object@KeyRateDuration = KeyRateDuration
            .Object@KeyRateConvexity = KeyRateConvexity
            #---- term structure slots
            .Object@tradedate = tradedate
            .Object@period = period
            .Object@date = date
            .Object@spotrate = spotrate
            .Object@forwardrate = forwardrate
            .Object@TwoYearFwd = TwoYearFwd
            .Object@TenYearFwd = TenYearFwd
            #--- prepayment assumption
            .Object@PrepayAssumption = PrepayAssumption
            .Object@PPCStart = PPCStart
            .Object@PPCEnd = PPCEnd
            .Object@PPCSeasoning = PPCSeasoning
            #.Object@FirstPmtDate = FirstPmtDate
            #.Object@LastPmtDate = LastPmtDate
            #.Object@FinalPmtDate = FinalPmtDate
            #.Object@PmtDate = PmtDate
            .Object@LoanAge = LoanAge
            #.Object@Period = Period
            .Object@NoteRate = NoteRate
            .Object@MtgRateFwd = MtgRateFwd
            .Object@Incentive = Incentive
            .Object@SMM = SMM
            .Object@MDR = MDR
            #--- Mtg Scenario Set
            .Object@Scenario = Scenario
            
            return(.Object)
            callNextMethod(.Object,...)
          })


  # This function analyzes a standard pass through security and serves as the constructor function
  #--------------------------------------
#' Pass Through Analytics is the Analytic engine for a pass-through security
#' 
#' Analytic engine for pass through security returns pass through analytics object
#' @param bond.id A character string the bond id or cusip
#' @param original.bal A numeric value the original balance of the pass through
#' @param price A numeric value the price of the pass through  
#' @param trade.date A character string the trade date
#' @param settlement.date A character string the settlement date
#' @param method A character string indicating the fitting method ns = Nelson Siegel, dl = Diebold Lee,
#' sv = Severson, asv = adjusted Severson, cs = cubic spline (not yet implemented in Bond Lab).
#' For addiition details see the termstrc documentation.
#' @param scenario.set A character vector listing the scenarios to run
#' @param PrepaymentAssumption A character string the prepayment assumption used "MODEL", "PPC", or "CPR"
#' @param ... Optional values when "PPC" or "CPR" is used
#' @param begin.cpr A numeric value the beginning CPR assumption
#' @param end.cpr A numeric value the ending CPR assumption
#' @param seasoning.period A numeric value the length of the seasoning ramp
#' @param CPR A numeric value the CPR assumption
#' @examples 
#' \dontrun{PassThroughAnalytics(bond.id = "bondlabMBS55", original.bal = 100000, 
#' price = 107.5, trade.date = "01-10-2013", settlement.date = "01-13-2013", 
#' scenario.set = c("DA25", "NC", "UA50", "UA100", "UA150", "UA200"), PrepaymentAssumption = "MODEL")}

#' @export PassThroughAnalytics
  PassThroughAnalytics <- function (bond.id = "character", 
                                    original.bal = numeric(), 
                                    price = numeric(), 
                                    trade.date = "character", 
                                    settlement.date = "character", 
                                    method = "character", 
                                    scenario.set = vector(),
                                    PrepaymentAssumption = "character", 
                                    ..., 
                                    begin.cpr = numeric(), 
                                    end.cpr = numeric(), 
                                    seasoning.period = numeric(), 
                                    CPR = numeric()
                                 ) 
  {
      
  #Error Trap Settlement Date and Trade Date order.  
  #This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  
  #It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) {method = "ns"}
  
  #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
  Rate.Delta = .25
  
  # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
  
  #conn1 <-  gzfile(description = paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""), open = "rb")
  #bond.id <- readRDS(conn1)
  
  bond.id <- MBS(MBS.id = bond.id)
  
  #Call the desired curve from rates data folder
  #conn2 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
  rates.data <- Rates(trade.date = trade.date)
  
  #Call Mortgage Rate Functions
  #conn3 <- gzfile("~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
  MortgageRate <- MtgRate()
  
  
  #move this to line 257 redundent
  Burnout = bond.id@Burnout
  
  #Call Prepayment Model Tuning Parameters
  ModelTune <- ModelTune(bond.id = bond.id)
  
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)

  #Third if mortgage security call the prepayment model
  PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, 
                                               MortgageRate = MortgageRate,
                                               TermStructure = TermStructure, 
                                               PrepaymentAssumption = PrepaymentAssumption, 
                                               ModelTune = ModelTune, 
                                               Burnout = Burnout, 
                                               begin.cpr = begin.cpr, 
                                               end.cpr = end.cpr, 
                                               seasoning.period = seasoning.period, 
                                               CPR = CPR
                                               )
    
  #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, 
  #Duration, Convexity and CashFlow.
  MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                        original.bal = original.bal, 
                                        settlement.date = settlement.date, 
                                        price = price, 
                                        PrepaymentAssumption = PrepaymentAssumption)
  
  #The fifth step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  MortgageTermStructure <- MtgTermStructure(bond.id = bond.id, 
                                            original.bal = original.bal, 
                                            Rate.Delta = Rate.Delta, 
                                            TermStructure = TermStructure, 
                                            settlement.date = settlement.date, 
                                            principal = original.bal *  bond.id@MBSFactor, 
                                            price = price, 
                                            cashflow = MortgageCashFlow)

  spread.to.spot = MortgageTermStructure@SpotSpread
  proceeds = ((price/100) * (original.bal * bond.id@MBSFactor)) + MortgageCashFlow@Accrued
  
  # The sixth step in scenario based analysis
  Scenario <- Mtg.Scenario(scenario.set = scenario.set, 
                           bond.id = bond.id, 
                           MortgageRate = MortgageRate, 
                           original.bal = original.bal, 
                           trade.date = trade.date, 
                           settlement.date = settlement.date, 
                           price = price, 
                           proceeds = proceeds, 
                           spot.spread = spread.to.spot, 
                           PrepaymentAssumption = "MODEL")
  
  closeAllConnections()
  
  new("PassThroughAnalytics", 
      Cusip = bond.id@Cusip,
      ID = bond.id@ID,
      BondType = bond.id@BondType,
      Sector = bond.id@Sector,
      Coupon = bond.id@Coupon,
      IssueDate = bond.id@IssueDate,
      DatedDate = bond.id@DatedDate,
      Maturity = bond.id@Maturity,
      LastPmtDate = bond.id@LastPmtDate,
      NextPmtDate = bond.id@NextPmtDate,
      PaymentDelay = bond.id@PaymentDelay,
      Moody = bond.id@Moody,
      SP = bond.id@SP,
      BondLab  = bond.id@BondLab,
      Frequency = bond.id@Frequency,
      BondBasis = bond.id@BondBasis,
      GWac = bond.id@GWac,
      AmortizationType = bond.id@AmortizationType,
      AmortizationTerm = bond.id@AmortizationTerm,
      Index = bond.id@Index,
      Margin = bond.id@Margin,
      FirstPmtDate = bond.id@FirstPmtDate,
      FinalPmtDate = bond.id@FinalPmtDate,
      Servicing = bond.id@Servicing,
      PMI = bond.id@PMI,
      Gfee = bond.id@Gfee,
      InitialInterest = bond.id@InitialInterest,
      InterestOnlyPeriod = bond.id@InterestOnlyPeriod,
      FirstPrinPaymentDate = bond.id@FirstPrinPaymentDate,
      BalloonPmt = bond.id@BalloonPmt,
      BalloonDate = bond.id@BalloonDate,
      MBSFactor = bond.id@MBSFactor,
      Model = bond.id@Model,
      Burnout = bond.id@Burnout,
      SATO = bond.id@SATO,
      #--- mortgage cashflow slots
      Price = MortgageCashFlow@Price,
      Accrued = MortgageCashFlow@Accrued,
      YieldToMaturity = MortgageCashFlow@YieldToMaturity,
      WAL = MortgageCashFlow@WAL,
      ModDuration = MortgageCashFlow@ModDuration,
      Convexity = MortgageCashFlow@Convexity,
      Period = MortgageCashFlow@Period,
      PmtDate = MortgageCashFlow@PmtDate,
      TimePeriod = MortgageCashFlow@TimePeriod,
      BeginningBal = MortgageCashFlow@BeginningBal,
      MonthlyPmt = MortgageCashFlow@MonthlyPmt,
      MonthlyInterest = MortgageCashFlow@MonthlyInterest,
      PassThroughInterest = MortgageCashFlow@PassThroughInterest,
      ScheduledPrin = MortgageCashFlow@ScheduledPrin,
      PrepaidPrin = MortgageCashFlow@PrepaidPrin,
      EndingBal = MortgageCashFlow@EndingBal,
      ServicingIncome = MortgageCashFlow@ServicingIncome,
      PMIPremium = MortgageCashFlow@PMIPremium,
      GFeePremium = MortgageCashFlow@GFeePremium,  
      TotalCashFlow = MortgageCashFlow@TotalCashFlow,
      #---- mortgage term structure slots
      SpotSpread = MortgageTermStructure@SpotSpread,
      EffDuration = MortgageTermStructure@EffDuration,
      EffConvexity = MortgageTermStructure@EffConvexity,
      KeyRateTenor = MortgageTermStructure@KeyRateTenor,
      KeyRateDuration = MortgageTermStructure@KeyRateDuration,
      KeyRateConvexity = MortgageTermStructure@KeyRateConvexity,
      #---- term structure
      tradedate = TermStructure@tradedate,
      period = TermStructure@period,
      date = TermStructure@date,
      spotrate = TermStructure@spotrate,
      forwardrate = TermStructure@forwardrate,
      TwoYearFwd = TermStructure@TwoYearFwd,
      TenYearFwd = TermStructure@TenYearFwd,
      #--- prepayment assumption 
      PrepayAssumption = PrepaymentAssumption@PrepayAssumption,
      PPCStart = PrepaymentAssumption@PPCStart,
      PPCEnd = PrepaymentAssumption@PPCEnd,
      PPCSeasoning = PrepaymentAssumption@PPCSeasoning,
      #FirstPmtDate = "character",
      #LastPmtDate = "character",
      #FinalPmtDate = "character",
      #PmtDate = "character",
      LoanAge = PrepaymentAssumption@LoanAge,
      #Period = "numeric",
      NoteRate = PrepaymentAssumption@NoteRate,
      MtgRateFwd = PrepaymentAssumption@MtgRateFwd,
      Incentive = PrepaymentAssumption@Incentive,
      SMM = PrepaymentAssumption@SMM,
      MDR = PrepaymentAssumption@MDR,
      #--- Mtg Scenario Set
      Scenario = Scenario@Scenario) 
  }

  setGeneric("PassThroughAnalytics", function (bond.id = "character", 
                                               original.bal = numeric(), 
                                               price = numeric(), 
                                               trade.date = "character", 
                                               settlement.date = "character", 
                                               method = "character", 
                                               scenario.set = vector(),
                                               PrepaymentAssumption = "character", 
                                               ..., 
                                               begin.cpr = numeric(), 
                                               end.cpr = numeric(), 
                                               seasoning.period = numeric(), 
                                               CPR = numeric()) 
             {standardGeneric("PassThroughAnalytics")})
  