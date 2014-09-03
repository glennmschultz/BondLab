
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
           SATO = "numeric"
         ))

setClass("MortgageCashFlows",
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
           #SMM = "numeric",
           PrepaidPrin = "numeric",
           EndingBal = "numeric",
           ServicingIncome = "numeric",
           PMIPremium = "numeric",
           GFeePremium = "numeric",  
           TotalCashFlow = "numeric"))


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
           SMM = "numeric"))

setClass("MortgageKeyRate",
         representation(
           SpotSpread = "numeric",   
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric"))


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
           SMM = "numeric"))


setClass("PrepaymentModel",
         representation(
           Turnover = "numeric",
           Seasoning = "function",
           Seasonality = "function",
           BorrowerIncentive = "function",
           Burnout = "function"))

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
           Burnout.beta.2 = "numeric"))

setClass("MortgageRate",
         representation(
           yr30 = "function",
           yr15 = "function"))

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
           HorizonReturn = "numeric"))

setClass("Mtg.ScenarioSet",
         representation(
           Scenario = "list"))

