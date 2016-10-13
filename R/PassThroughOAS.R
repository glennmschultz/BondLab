
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc. 

  #' @include PassThroughConstructor.R MortgageCashFlow.R MortgageOAS.R
  NULL
  
  #' An S4 class containing the classes MBSDetails, MortgageCashFlow, 
  #' and MortgageOAS
  #' 
  #' @exportClass PassThroughOAS
  setClass("PassThroughOAS",
         representation(),
         contains = c("MBSDetails", 
                      "MortgageCashFlow", 
                      "MortgageOAS"))


  setGeneric("PassThroughOAS", function(bond.id = "character", 
                                      trade.date = "character", 
                                      settlement.date = "character", 
                                      original.bal = numeric(), 
                                      price = numeric(), 
                                      sigma = numeric(), 
                                      paths = numeric())
  {standardGeneric("PassThroughOAS")})
  
  setMethod("initialize",
          signature("PassThroughOAS"),
          function(.Object,
                   ...)
          {callNextMethod(.Object,
            ...)

          })


  #' The constructor function for the passthrough OAS object 
  #' 
  #' This function is the Pass Through OAS constructor function
  #' @param bond.id A character string the MBS cusip number
  #' @param trade.date A character string the trade date
  #' @param settlement.date A character string the settlement date
  #' @param original.bal A numeric value the original balance traded
  #' @param price A numeric value the trade price
  #' @param sigma A numeric value interest rate volatility assumption
  #' @param paths A numeric value the number of simulated paths
  #' @examples 
  #' \dontrun{
  #' PassThroughOAS(bond.id = "bondlabMBS4", trade.date = "01-10-2013", 
  #' settlement.date = "01-13-2013", 
  #' original.bal = 100000, price = 105.75, sigma = 0.015, paths = 200)} 
  #' @export PassThroughOAS
  PassThroughOAS <- function(bond.id = "character", 
                           trade.date = "character", 
                           settlement.date = "character", 
                           original.bal = numeric(), 
                           price = numeric(), 
                           sigma = numeric(), 
                           paths = numeric()){
  
  #Error Trap Settlement Date and Trade Date order.  
  #This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  
  #It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
                        
  Rate.Delta = rate.delta
  
  #The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
  bond.id <- MBS(MBS.id = as.character(bond.id))
  Burnout <- bond.id@Burnout
  
  #Call the desired curve from rates data folder
  rates.data <- Rates(trade.date = trade.date)
  short.rate <- as.numeric(rates.data[1,2])/yield.basis
  
  #Call Mortgage Rate Functions
  MortgageRate <- MtgRate()
    
  #Call Prepayment Model Tuning Parameters
  ModelTune <- ModelTune(bond.id = bond.id)
  
  #This section begins standard bond analysis based on the term structure fit from
  #The CIR Model Term Structure 0 volatiltiy assumption
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  delay = bond.id@PaymentDelay
  factor = bond.id@MBSFactor
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  bondbasis = bond.id@BondBasis
  
  pmtdate.interval = months.in.year/frequency
  
  #Compute the payment dates 
  pmtdate = as.Date(c(if(settlement.date == issue.date) 
  {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
  else 
  {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, 
                                          "months"))}), "%m-%d-%Y") + delay
  
  time.period <- BondBasisConversion(issue.date = issue.date, 
                                    start.date = start.date, 
                                    end.date = end.date, 
                                    settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, 
                                    nextpmt.date = pmtdate,
                                    type = bondbasis)
  
  #Compute the payment dates 
  pmtdate = as.Date(c(if(settlement.date == issue.date) 
  {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
  else 
  {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, 
                                          "months"))}), "%m-%d-%Y") + delay
  
  #Count the number of cashflows 
  #num.periods is the total number of cashflows to be received
  #num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  num.period = seq(1:num.periods)
  
  #Call OAS Term Strucuture to Pass to the Prepayment Model 

  Market.Fit <- CalibrateCIR(trade.date = trade.date, sigma = sigma)
  kappa  = Market.Fit$p1
  lambda = Market.Fit$p2
  theta  = Market.Fit$p3
  
  
  Sim.Rate <- CIRSim(shortrate = short.rate,
                      kappa = kappa,
                      theta = theta,
                      T = ((num.periods-1) / months.in.year),
                      step = 1/months.in.year,
                      sigma = 0,
                      N = 1)
  
  #Compute the continuously compounded rate
  Compounded.Rate <- cumprod(1 + Sim.Rate)
  
  #Compute the Spot Rate
  Spot.Rate <- (((Compounded.Rate ^ (1/ time.period))^(1/months.in.year))-1)
  Two.Year.Fwd <- as.vector(CIRBondPrice(shortrate = as.numeric(Sim.Rate), 
                                      kappa = kappa, 
                                      lambda = lambda, 
                                      theta = theta, sigma = 0, 
                                      T = 2, step = 0, 
                                      result = "y") * yield.basis)
  
  Ten.Year.Fwd <- as.vector(CIRBondPrice(shortrate = as.numeric(Sim.Rate), 
                                      kappa = kappa, 
                                      lambda = lambda, 
                                      theta = theta, 
                                      sigma = 0, 
                                      T = 10, 
                                      step = 0, 
                                      result = "y") * yield.basis)

  TermStructure <- new("TermStructure",
                       TradeDate = as.character(trade.date),
                       Period = as.numeric(time.period),
                       Date = as.character(as.Date(pmtdate, origin = "1970-01-01")),
                       SpotRate = as.numeric(Spot.Rate),
                       ForwardRate = as.numeric(Sim.Rate),
                       TwoYearFwd = as.numeric(Two.Year.Fwd),
                       TenYearFwd = as.numeric(Ten.Year.Fwd))
  
  
  #Third if mortgage security call the prepayment model
  PrepaymentAssumption <- PrepaymentModel(bond.id = bond.id, 
                                               MortgageRate = MortgageRate, 
                                               TermStructure = TermStructure, 
                                               PrepaymentAssumption = "MODEL", 
                                               ModelTune = ModelTune, 
                                               Burnout = Burnout)
  
  #The fourth step is to call the bond cusip details and calculate 
  #Bond Yield to Maturity, Duration, Convexity and CashFlow.
  MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                       original.bal = original.bal, 
                                       settlement.date = settlement.date, 
                                       price = price, 
                                       PrepaymentAssumption = PrepaymentAssumption)
  
  #Calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the MtgTermStructureFunction this creates the class BondTermStructure
  #MortgageTermStructure <- MtgTermStructure(bond.id = MortgageCashFlow, original.bal = original.bal, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
  #settlement.date = settlement.date, principal = original.bal *  MortgageCashFlow@MBSFactor, price = price, cashflow = MortgageCashFlow)
  #End of 0 volatility term structure fit analysis
  
  #This section begins the OAS analysis
  MortgageOAS  <- Mortgage.OAS(bond.id = bond.id@ID, 
                               trade.date = trade.date, 
                               settlement.date = settlement.date, 
                               original.bal = original.bal, 
                               price = price, 
                               sigma = sigma, 
                               paths = paths) 

  
  new("PassThroughOAS",
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
      Term = bond.id@Term,
      WALA = bond.id@WALA,
      WAM = bond.id@WAM,
      PaymentDelay = bond.id@PaymentDelay,
      Moody = bond.id@Moody,
      SP = bond.id@SP,
      BondLab  = bond.id@BondLab,
      Frequency = bond.id@Frequency,
      BondBasis = bond.id@BondBasis,
      GWac = bond.id@GWac,
      OrigLoanBal = bond.id@OrigLoanBal,
      OrigLTV = bond.id@OrigLTV,
      AmortizationType = bond.id@AmortizationType,
      AmortizationTerm = bond.id@AmortizationTerm,
      Index = bond.id@Index,
      Margin = bond.id@Margin,
      FirstPmtDate = bond.id@FirstPmtDate,
      FinalPmtDate = bond.id@FinalPmtDate,
      Servicing = bond.id@Servicing,
      PMI = bond.id@PMI,
      InitialInterest = bond.id@InitialInterest,
      InterestOnlyPeriod = bond.id@InterestOnlyPeriod,
      FirstPrinPaymentDate = bond.id@FirstPrinPaymentDate,
      BalloonPmt = bond.id@BalloonPmt,
      BalloonDate = bond.id@BalloonDate,
      MBSFactor = bond.id@MBSFactor,
      OriginalBal = bond.id@MBSFactor,
      CurrentBal = bond.id@OrigLTV,
      Model = bond.id@Model,
      Burnout = bond.id@Burnout,
      SATO = bond.id@SATO,
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
      DefaultedPrin = MortgageCashFlow@DefaultedPrin,
      LossAmount = MortgageCashFlow@LossAmount,
      RecoveredAmount = MortgageCashFlow@RecoveredAmount,
      EndingBal = MortgageCashFlow@EndingBal,
      ServicingIncome = MortgageCashFlow@ServicingIncome,
      PMIPremium = MortgageCashFlow@PMIPremium,
      GFeePremium = MortgageCashFlow@GFeePremium,  
      TotalCashFlow = MortgageCashFlow@TotalCashFlow,
      OAS = MortgageOAS@OAS,
      ZeroVolSpread = MortgageOAS@ZeroVolSpread,
      SpreadToCurve = MortgageOAS@SpreadToCurve,
      PriceDist = MortgageOAS@PriceDist,
      PathSpread = MortgageOAS@PathSpread,
      PathWAL = MortgageOAS@PathWAL,
      PathModDur = MortgageOAS@PathModDur,
      PathYTM = MortgageOAS@PathYTM)
  }

