
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.


  #' @include BondConstructor.R BondCashFlow.R BondKeyRate.R TermStructure.R
  NULL
  
  setClass("BondAnalytics", 
           representation(),
           contains = c("BondDetails", 
                        "BondCashFlows", 
                        "BondTermStructure", 
                        "TermStructure"))
  setMethod("initialize",
          signature("BondAnalytics"),
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
                   Moody = "character",
                   SP = "character",
                   BondLab  = "character",
                   Frequency = "numeric",
                   BondBasis = "character",
                   Callable = "character",
                   Putable = "character",
                   SinkingFund = "character",
                   # ---------- Bond Cash Flow Object ------
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
                   TotalCashFlow = "numeric",
                   # ---------- Bond Term Structure ------
                   SpotSpread = "numeric",   
                   EffDuration = "numeric",
                   EffConvexity = "numeric",
                   KeyRateTenor = "numeric",
                   KeyRateDuration = "numeric",
                   KeyRateConvexity = "numeric",
                   # ----------- Term Structure ---------
                   TradeDate = "character",
                   #Period = "numeric",
                   Date = "character",
                   SpotRate = "numeric",
                   ForwardRate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric",
                   ...){
            callNextMethod(.Object,
                           Cusip = Cusip,
                           ID = ID,
                           BondType = BondType,
                           Sector = Sector,
                           Coupon = Coupon,
                           IssueDate = IssueDate,
                           DatedDate = DatedDate,
                           Maturity = Maturity,
                           LastPmtDate = LastPmtDate,
                           NextPmtDate = NextPmtDate,
                           Moody = Moody,
                           SP = SP,
                           BondLab  = BondLab,
                           Frequency = Frequency,
                           BondBasis = BondBasis,
                           Callable = Callable,
                           Putable = Putable,
                           SinkingFund = SinkingFund,
                           Price = Price,
                           Accrued = Accrued,
                           YieldToMaturity = YieldToMaturity,
                           WAL = WAL,
                           ModDuration = ModDuration,
                           Convexity = Convexity,
                           Period = Period,
                           PmtDate = PmtDate,
                           TimePeriod = TimePeriod,
                           PrincipalOutstanding = PrincipalOutstanding,
                           CouponPmt = CouponPmt,
                           TotalCashFlow = TotalCashFlow,
                           SpotSpread = SpotSpread,
                           EffDuration = EffDuration,
                           EffConvexity = EffConvexity,
                           KeyRateTenor = KeyRateTenor,
                           KeyRateDuration = KeyRateDuration,
                           KeyRateConvexity = KeyRateConvexity,
                           TradeDate = TradeDate,
                           Period = Period,
                           Date = Date,
                           SpotRate = SpotRate,
                           ForwardRate = ForwardRate,
                           TwoYearFwd = TwoYearFwd,
                           TenYearFwd = TenYearFwd,
            ...)
            })
  
  setGeneric("BondAnalytics", function (bond.id = "character", 
                                        principal = numeric(), 
                                        price = numeric(), 
                                        trade.date = "character", 
                                        settlement.date = "character", 
                                        method = "character")
  {standardGeneric("BondAnalytics")})

  #'Bond Analytics is the Analytic engine for a standard non-callable bond 
  #'
  #'Analytic engine for standard bond (non-mortgage) analysis returns 
  #'Bond Analytics object
  #'@param bond.id A character string the cusip number or id of the bond
  #'@param principal  A numeric value the principal amount of the bond
  #'@param price A numeric value the price of the bond
  #'@param trade.date A character string the trade date
  #'@param settlement.date A character string the settlement date
  #'@param method A character string the method used to fit the term 
  #'structure passed to the package termstruc
  #'@examples
  #'\dontrun{BondAnalytics(bond.id = "bondlab10", principal = 1000, price = 100, 
  #'trade.date = "01-10-2013", settlement.date = "01-16-2013")}
  #'@export
  BondAnalytics <- function (bond.id = "character",
                             principal = numeric(),
                             price = numeric(), 
                             trade.date = "character", 
                             settlement.date = "character", 
                             method = "character"){
  # Error Trap Settlement Date and Trade Date order.  This is not done 
  # in the Error Trap Function because that function is 
  # to trap errors in bond information that is passed into the functions.  It is
  # trapped here because this is the first use of trade date

  if(trade.date > settlement.date) stop 
    ("Trade Date Must be less than settlement date")

  #Default method for TermStructure
  if(missing(method)) method = "ns"

  Rate.Delta = rate.delta
  # The first step is to read in the Bond Detail
  bond.id <- Bond(Bond.id = bond.id)
  
  #Call the desired curve from rates data folder
  trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data <- Rates(trade.date = trade.date)

  # Fit the term structure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
       
  # The next step is to call the bond cusip details and calculate Bond Yield to
  # Maturity, Duration, Convexity and CashFlow. The BondCashFlows function this
  # creates the class BondCashFlows are held in class BondCashFlows
  BondCashFlow <- BondCashFlows(bond.id = bond.id, 
                                principal = principal, 
                                settlement.date = settlement.date, 
                                price = price)
       
  # The final step is to calculate effective duration, convexity, and key rate
  # durations and key rate convexities. This is done with the 
  # BondTermStructureFunction this creates the class BondTermStructure
  BondTermStructure <- BondTermStructure(bond.id = bond.id, 
                                         Rate.Delta = Rate.Delta, 
                                         TermStructure = TermStructure, 
                                         principal = principal, 
                                         price = price, cashflow = BondCashFlow)
  new("BondAnalytics",
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
      Moody = bond.id@Moody,
      SP = bond.id@SP,
      BondLab  = bond.id@BondLab,
      Frequency = bond.id@Frequency,
      BondBasis = bond.id@BondBasis,
      Callable = bond.id@Callable,
      Putable = bond.id@Putable,
      SinkingFund = bond.id@SinkingFund,
  # ---------- Bond Cash Flow Object ------
  Price = BondCashFlow@Price,
  Accrued = BondCashFlow@Accrued,
  YieldToMaturity = BondCashFlow@YieldToMaturity,
  WAL = BondCashFlow@WAL,
  ModDuration = BondCashFlow@ModDuration,
  Convexity = BondCashFlow@Convexity,
  Period = BondCashFlow@Period,
  PmtDate = BondCashFlow@PmtDate,
  TimePeriod = BondCashFlow@TimePeriod,
  PrincipalOutstanding = BondCashFlow@PrincipalOutstanding,
  CouponPmt = BondCashFlow@CouponPmt,
  TotalCashFlow = BondCashFlow@TotalCashFlow,
  # ---------- Bond Term Structure ------
  SpotSpread = BondTermStructure@SpotSpread,
  EffDuration = BondTermStructure@EffDuration,
  EffConvexity = BondTermStructure@EffConvexity,
  KeyRateTenor = BondTermStructure@KeyRateTenor,
  KeyRateDuration = BondTermStructure@KeyRateDuration,
  KeyRateConvexity = BondTermStructure@KeyRateConvexity,
  # ----------- Term Structure ---------
  TradeDate = TermStructure@TradeDate,
  #Period = TermStructure@Period,
  Date = TermStructure@Date,
  SpotRate = TermStructure@SpotRate,
  ForwardRate = TermStructure@ForwardRate,
  TwoYearFwd = TermStructure@TwoYearFwd,
  TenYearFwd = TermStructure@TenYearFwd)
  }