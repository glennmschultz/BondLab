# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Bond Lab Technologies, Inc.
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

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
                   tradedate = "character",
                   period = "numeric",
                   date = "character",
                   spotrate = "numeric",
                   forwardrate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric")
            {
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
            .Object@Moody = Moody
            .Object@SP = SP
            .Object@BondLab  = BondLab
            .Object@Frequency = Frequency
            .Object@BondBasis = BondBasis
            .Object@Callable = Callable
            .Object@Putable = Putable
            .Object@SinkingFund = SinkingFund
            .Object@Price = Price
            .Object@Accrued = Accrued
            .Object@YieldToMaturity = YieldToMaturity
            .Object@WAL = WAL
            .Object@ModDuration = ModDuration
            .Object@Convexity = Convexity
            .Object@Period = Period
            .Object@PmtDate = PmtDate
            .Object@TimePeriod = TimePeriod
            .Object@PrincipalOutstanding = PrincipalOutstanding  
            .Object@CouponPmt = CouponPmt
            .Object@TotalCashFlow = TotalCashFlow
            .Object@SpotSpread = SpotSpread   
            .Object@EffDuration = EffDuration
            .Object@EffConvexity = EffConvexity
            .Object@KeyRateTenor = KeyRateTenor
            .Object@KeyRateDuration = KeyRateDuration
            .Object@KeyRateConvexity = KeyRateConvexity
            .Object@tradedate = tradedate
            .Object@period = period
            .Object@date = date
            .Object@spotrate = spotrate
            .Object@forwardrate = forwardrate
            .Object@TwoYearFwd = TwoYearFwd
            .Object@TenYearFwd = TenYearFwd
            
            return(.Object)
            callNextMethod(.Object,...)
            
          })

      #'Bond Analytics is the Analytic engine for a standard non-callable bond 
      #'
      #'Analytic engine for standard bond (non-mortgage) analysis returns Bond Analytics object
      #'@param bond.id A character string the cusip number or id of the bond
      #'@param principal  A numeric value the principal amount of the bond
      #'@param price A numeric value the price of the bond
      #'@param trade.date A character string the trade date
      #'@param settlement.date A character string the settlement date
      #'@param method A character string the method used to fit the term structure passed to the package termstruc
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
        #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
        #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
        if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
        
        #Default method for TermStructure
        if(missing(method)) method = "ns"
        
        #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
        Rate.Delta = 1
        
        # The first step is to read in the Bond Detail
        bond.id <- Bond(Bond.id = bond.id)
        #Call the desired curve from rates data folder
        trade.date = as.Date(trade.date, "%m-%d-%Y")
        rates.data <- Rates(trade.date = trade.date)
        
       # Fit the term structure
        TermStructure <- TermStructure(rates.data = rates.data, 
                                       method = method)
       
       #The next step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
       #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
       BondCashFlow <- BondCashFlows(bond.id = bond.id, 
                                     principal = principal, 
                                     settlement.date = settlement.date, 
                                     price = price)
       
       #The final step is to calculate effective duration, convexity, and key rate durations and key rate convexities
       #This is done with the BondTermStructureFunction this creates the class BondTermStructure
       BondTermStructure <- BondTermStructure(bond.id = bond.id, 
                                              Rate.Delta = Rate.Delta, 
                                              TermStructure = TermStructure, 
                                              principal = principal, 
                                              price = price, cashflow = BondCashFlow)
       
       closeAllConnections()
       
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
           tradedate = TermStructure@tradedate,
           period = TermStructure@period,
           date = TermStructure@date,
           spotrate = TermStructure@spotrate,
           forwardrate = TermStructure@forwardrate,
           TwoYearFwd = TermStructure@TwoYearFwd,
           TenYearFwd = TermStructure@TenYearFwd)
      }

  setGeneric("BondAnalytics", function (bond.id = "character", 
                                        principal = numeric(), 
                                        price = numeric(), 
                                        trade.date = "character", 
                                        settlement.date = "character", 
                                        method = "character")
    {standardGeneric("BondAnalytics")})