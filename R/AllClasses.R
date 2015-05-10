  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
  # Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
  # the book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

  # --- The following classes define standard bond analytics
  
  setClass("BondDetails",
         representation(
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
           Moody = "character",
           SP = "character",
           BondLab  = "character",
           Frequency = "numeric",
           BondBasis = "character",
           Callable = "character",
           Putable = "character",
           SinkingFund = "character"))

  setClass("BondCashFlows",
         representation(
           Price = "numeric",
           Accrued = "numeric",
           YieldToMaturity = "numeric",
           WAL = "numeric",
           ModDuration = "numeric",
           Convexity = "numeric",
           Period = "numeric",
           PmtDate = "character",
           TimePeriod = "numeric",
           PrincipalOutstanding = "numeric",  
           CouponPmt = "numeric",
           TotalCashFlow = "numeric"))

  setClass("BondTermStructure",
         representation(
           SpotSpread = "numeric",   
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric"))

  #--- The folllowing classes define standard Mortgage Passthrough analytics

  setClass("MBSDetails", 
         representation(
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
           OrigLoanBal = "numeric",
           OrigLTV = "numeric",
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
           SATO = "numeric"))


  setClass("MortgageTermStructure",
         representation(
           SpotSpread = "numeric",   
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric"))
  

  setClass("MortgageCashFlow",
         representation(
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
           DefaultedPrin = "numeric",
           LossAmount = "numeric",
           RecoveredAmount = "numeric",
           EndingBal = "numeric",
           ServicingIncome = "numeric",
           PMIPremium = "numeric",
           GFeePremium = "numeric",  
           TotalCashFlow = "numeric"))
  

  setClass("MortgageOAS",
         representation(
           OAS = "numeric",
           ZVSpread = "numeric",
           SpreadToCurve = "numeric",
           PriceDist = "vector",
           PathSpread = "vector",
           PathWAL = "vector",
           PathModDur = "vector",
           PathYTM = "vector"))


  setClass("Scenario",
         representation(
           Name = "character",
           Type = "character",
           Horizon = "character",
           ShiftType = "character",
           Shiftbps = "numeric",
           Formula = "function"
         ))

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
  
  setClass("Mtg.ScenarioSet",
         representation(
           Scenario = "list"))
  
  setClass("PrepaymentAssumption",
         representation(
           PrepayAssumption = "character",
           PPCStart = "numeric",
           PPCEnd = "numeric",
           PPCSeasoning = "numeric",
           FirstPmtDate = "character",
           LastPmtDate = "character",
           FinalPmtDate = "character",
           PmtDate = "character",
           LoanAge = "numeric",
           Period = "numeric",
           NoteRate = "numeric",
           MtgRateFwd = "numeric",
           Incentive = "numeric",
           SMM = "numeric",
           MDR = "numeric",
           Severity = "numeric"))
  
  setClass("DollarRoll",
         representation(
           SettlementDate = "character",
           FwdSettlementDate = "character",  
           GrossCoupon = "numeric",
           NetCoupon = "numeric",
           OriginalTerm = "numeric",
           RemainingTerm = "numeric",
           OrigBalance = "numeric",
           CurrentBalance = "numeric",
           Price = "numeric",
           PrincipalProceeds = "numeric",
           Accrued = "numeric",
           TotalProceeds = "numeric",
           DaysInterest = "numeric",
           ReinvestmentIncome = "numeric",
           ScheduledPrin = "numeric",
           PrepaidPrin = "numeric",
           PassThroughInterest = "numeric",
           FutureValueHold = "numeric",
           RemainingBalance = "numeric",
           FuturePrincipalProceeds = "numeric",
           FwdAccrued = "numeric",
           Drop = "numeric",
           FwdPrice = "numeric",
           FinanceRate = "numeric",
           ReinvestmentRate = "numeric",
           HoldorRoll = "character",
           Advantage = "numeric",
           FutureValueRoll = "numeric",
           DiscValueofCarry = "numeric",
           FutureValuePrinCarry = "numeric",
           TotalFutureValue = "numeric",
           DropImpliedValue = "numeric")) 

  # --- The following classes define rates and Prepayment model tune classes
  # --- these classes are used to pass term strucuture information and prepayment model
  # --- tuning paramaters  

  setClass("TermStructure",
         representation(
           tradedate = "character",
           period = "numeric",
           date = "character",
           spotrate = "numeric",
           forwardrate = "numeric",
           TwoYearFwd = "numeric",
           TenYearFwd = "numeric"
         ))
  #-- This is protype of function class to hold the prepayment model
  setClass("PrepaymentModel",
         representation(
           Turnover = "numeric",
           Seasoning = "function",
           Seasonality = "function",
           BorrowerIncentive = "function",
           Burnout = "function"
         ))

  setClass("PrepaymentModelTune",
         representation(
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
         ))

  setClass("MortgageRate",
         representation(
           yr30 = "function",
           yr15 = "function"
         ))
  
  # This is a prototype class for updated loan to value ratio calculation
  setClass("UpdatedLTV",
           representation(
             ULTV = "function"))

  # ----- The following classes define rate of return and valuation classes

  setClass("RateofReturn",
         representation(
           PmtDate = "character",
           Period = "numeric",
           ReinvestmentRate = "numeric",
           ReceivedCF = "numeric",
           ReInvestmentIncome = "numeric",
           RemainingCF = "numeric",
           HorizonSpread = "numeric"))

  #------ These classes the superclasses and providing the results of analytic runs

  setClass("BondAnalytics", 
           representation(),
           contains = c("BondDetails", 
                        "BondCashFlows", 
                        "BondTermStructure", 
                        "TermStructure"))
  
  setClass("DollarRollAnalytics",
           representation(),
           contains = c("MBSDetails",
                        "DollarRoll"))

  setClass("PassThroughAnalytics", 
           representation(),
           contains = c("MBSDetails", 
                        "MortgageCashFlow", 
                        "MortgageTermStructure", 
                        "TermStructure", 
                        "PrepaymentAssumption", 
                        "Mtg.ScenarioSet"))

  setClass("PassThroughOAS",
           representation(),
           contains = c("MBSDetails", 
                      "MortgageCashFlow", 
                      "MortgageOAS"))

  # REMIC Classes, these classes define a REMIC
  # In all, there are eight classes that define a REMIC structure
  
  # ==== REMIC CashFlow Cashflow Class =======================
  setClass("REMICCashFlow",
           representation(
             DealName = "character",
             TrancheName = "character",
             TrancheNumber = "character",
             Price = "numeric",
             PrincipalProceeds = "numeric",
             Accrued = "numeric",
             YieldToMaturity = "numeric",
             WAL = "numeric",
             ModDuration = "numeric",
             Convexity = "numeric",
             Period = "numeric",
             PmtDate = "character",
             TimePeriod = "numeric",
             Interest = "numeric",
             Principal = "numeric",
             TotalCashFlow = "numeric"))
  
  setClass("REMICTermStructure",
           representation(
             SpotSpread = "numeric",
             EffDuration = "numeric",
             EffConvexity = "numeric",
             KeyRateTenor = "numeric",
             KeyRateDuration = "numeric",
             KeyRateConvexity = "numeric"))

  # ==== The RAID class is the REMIC at issuance disclosure ===
  setClass("RAID",
         representation(
           DealName = "character", 
           Issuer = "character",
           DealPriceDate = "character",
           DealSettlementDate = "character",
           Underwriter = "character",
           NumberofTranches = "numeric",
           NumberPacSchedules = "numeric",
           NumberofGroups = "numeric",
           DealSize = "numeric",
           CollateralAmount = "numeric"))

  # ============== This Class is the Tranche Class Tranche Belongs to Deal =========================
  # ======== This Class contains all Tranche details that are related to the REMIC =================
  setClass("TrancheDetails",
         representation(
           DealName = "character",
           TrancheNumber = "character",
           TrancheName = "character",
           TranchePrincipal = "character",
           TrancheInterest = "character",
           TranchePrincipalDesc = "character",
           TrancheInterestDesc = "character",
           Cusip = "character",
           TrancheOrigBal = "numeric",
           TrancheDatedDate = "character",
           TrancheFirstPmtDate = "character",
           TrancheLastPmtDate = "character",
           TrancheNextPmtDate = "character",
           TrancheCoupon = "numeric",
           Delay = "numeric",
           PrinPmtFrequency = "numeric",
           InterestPmtFrequency = "numeric",
           FloaterIndex = "character",
           FloaterMargin = "numeric",
           FloaterCap = "numeric",
           FloaterFloor = "numeric",
           FloaterFormula = "function",
           PacLowBand = "numeric",
           PacHighBand = "numeric",
           Group = "numeric",
           Schedule = "logical",
           Fixed = "logical"
         ))


  # ===== The Tranches Class is a list that holds the above tranche information
  # ===== for the REMIC which are strucutred with many tranches GSEs deliver tranche detail

  setClass("Tranches",
         representation(
           Tranches = "list"))

  # ==== Collateral Class is the representation of the collateral underlying a REMIC transaction

  setClass("Collateral",
         representation(
           Group = "numeric",
           Cusip = "list",
           OrigBal = "list"))

  # ========== Collateral Group Class is an aggregator of the collateral class ================
  # This function assembles multiple collateral groups into a list of collateral groups
  # building the collateral groups for the entire deal structure

  setClass("CollateralGroup",
         representation(
           Group = "list"))
  
  # ========= Schedules is the projected schedule for a PAC/TAC Schedule
  setClass("Schedule",
          representation(
            DealName = "character",
            Group = "numeric",
            PmtDate = "character",
            Balance = "numeric",
            ScheduledPmt = "numeric"))


  # ======== This class is the REMIC factor files and belongs to tranche information ==================
  # REMIC Disclosure Month End (RDME) Class stores the tranch factor data and is part of the assembly of the REMIC

  setClass("RDME",
         representation(
           Cusip = "character",
           PaymentDate = "character",
           Coupon = "numeric",
           Factor = "numeric"))

  # =============== The TrancheFactors class is an aggregator class ===================
  # ============ The class aggregates the RDME classes for each associated trance ====

  setClass("TrancheFactors",
         representation(
           FactorData = "list"))

  #========== Superclass REMIC structure constructor for REMIC which will be called by the waterfall ==========
  setClass("REMICStructure",
         representation(),
         contains = c("RAID", 
                      "Tranches", 
                      "CollateralGroup", 
                      "TrancheFactors")) 
  
  #========= Superclass REMIC Analytics constructor for REMIC Analytics ===========================
  setClass("REMICAnalytics",
           representation(),
           contains = c("TrancheDetails",
                        "REMICCashFlow",
                        "REMICTermStructure"))

