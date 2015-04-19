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
            signature("DollarRollAnalytics"),
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
                     DropImpliedValue = "numeric"){
              
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
              .Object@SettlementDate = SettlementDate
              .Object@FwdSettlementDate = FwdSettlementDate  
              .Object@GrossCoupon = GrossCoupon
              .Object@NetCoupon = NetCoupon
              .Object@OriginalTerm = OriginalTerm
              .Object@RemainingTerm = RemainingTerm
              .Object@OrigBalance = OrigBalance
              .Object@CurrentBalance = CurrentBalance
              .Object@Price = Price
              .Object@PrincipalProceeds = PrincipalProceeds
              .Object@Accrued = Accrued
              .Object@TotalProceeds = TotalProceeds
              .Object@DaysInterest = DaysInterest
              .Object@ReinvestmentIncome = ReinvestmentIncome
              .Object@ScheduledPrin = ScheduledPrin
              .Object@PrepaidPrin = PrepaidPrin
              .Object@PassThroughInterest = PassThroughInterest
              .Object@FutureValueHold = FutureValueHold
              .Object@RemainingBalance = RemainingBalance
              .Object@FuturePrincipalProceeds = FuturePrincipalProceeds
              .Object@FwdAccrued = FwdAccrued
              .Object@Drop = Drop
              .Object@FwdPrice = FwdPrice
              .Object@FinanceRate = FinanceRate
              .Object@ReinvestmentRate = ReinvestmentRate
              .Object@HoldorRoll = HoldorRoll
              .Object@Advantage = Advantage
              .Object@FutureValueRoll = FutureValueRoll
              .Object@DiscValueofCarry = DiscValueofCarry
              .Object@FutureValuePrinCarry = FutureValuePrinCarry
              .Object@TotalFutureValue = TotalFutureValue
              .Object@DropImpliedValue = DropImpliedValue
              
                return(.Object)
                callNextMethod(.Object,...)})


  #----------------------------------
  #Agency Mortgage Dollar Roll Analytics is the constructor function for Dollar Roll
  #' A constructor of Dollar Roll Analytics object Analysis of the one-month Dollar Roll
  #' 
  #' Analysis of the one-month dollar roll for a pass through or cohort representation
  #' @param bond.id A character string the MBS cusip or bond id
  #' @param original.bal A numeric value the MBS original balance
  #' @param price A numeric value the price of the MBS pass-through
  #' @param drop A numeric value the quoted drop
  #' @param trade.date A character string the trade date
  #' @param settlement.date A character string the settlement date
  #' @param fwd.settlement.date A character string the forward settlement date of the roll
  #' @param reinvestment.rate A numeric value the investor's reinvestment rate
  #' @param finance.rate A numeric value the investor's alternative finance or repo rate
  #' @param method A character string the method passed to termstrc to fit the term structure
  #' @param PrepaymentAssumption A character string the model used to value the dollar roll may be CPR, PPC, or MODEL
  #' @param ... Optional values for input when using CPR or PPC
  #' @param begin.cpr optional, A numeric value the beginning cpr that must be specified when PPC prepayment assumption is used
  #' @param end.cpr optional, A numeric vlaue the ending cpr that must be specified  when PPC prepayment assumption is used
  #' @param seasoning.period optional, A numeric value the length of the PPC seasoning ramp that must be specified when PPC prepayment assumption is used
  #' @param CPR optional, A numeric value the CPR assumption that must be specified when CPR prepayment assumption is used
  #' @export
  DollarRollAnalytics <- function(bond.id = "character", 
                                  original.bal= numeric(), 
                                  price = numeric(), 
                                  drop = numeric(), 
                                  trade.date = "character", 
                                  settlement.date = "character", 
                                  fwd.settlement.date = "character", 
                                  reinvestment.rate = numeric(),  
                                  finance.rate = numeric(), 
                                  method = "ns", 
                                  PrepaymentAssumption = "character", 
                                  ...,
                                  begin.cpr = numeric(), 
                                  end.cpr = numeric(), 
                                  seasoning.period = numeric(), 
                                  CPR = numeric()) {
  
  #Error Trap Settlement Date and Trade Date order.  
  #This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  
  #It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) method = "ns"
  
  
  # Open bond.id connection

  bond.id <- MBS(MBS.id = bond.id)
  
  # Open connection to rates data 
  trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data = Rates(trade.date = trade.date)
  
  #Open Model Tune Connection
  ModelTune <-ModelTune(bond.id = bond.id)
  
  # Open Mortgage Rate Connection
  MortgageRate <- MtgRate()
  
  Burnout = bond.id@Burnout
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  # Third if mortgage security call the prepayment model
  PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, 
                                               TermStructure = TermStructure, 
                                               MortgageRate = MortgageRate,
                                               PrepaymentAssumption = PrepaymentAssumption, 
                                               ModelTune = ModelTune, 
                                               Burnout = Burnout, 
                                               begin.cpr = begin.cpr, 
                                               end.cpr = end.cpr, 
                                               seasoning.period = seasoning.period, 
                                               CPR = CPR)
  
  #The fourth step is to call the bond cusip details and calculate 
  #Bond Yield to Maturity, Duration, Convexity and CashFlow. 
  #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
  MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                         original.bal = original.bal, 
                                         settlement.date = settlement.date, 
                                         price = price, 
                                         PrepaymentAssumption = PrepaymentAssumption)
  
  DollarRoll <- DollarRoll(bond.id = bond.id, 
                           price = price, 
                           drop = drop, 
                           original.bal = original.bal, 
                           settlement.date = settlement.date, 
                           fwd.settlement.date = fwd.settlement.date, 
                           reinvestment.rate = reinvestment.rate, 
                           finance.rate = finance.rate, 
                           MortgageCashFlow = MortgageCashFlow)
  
  new("DollarRollAnalytics",
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
      SettlementDate = DollarRoll@SettlementDate,
      FwdSettlementDate = DollarRoll@FwdSettlementDate,
      GrossCoupon = DollarRoll@GrossCoupon,
      NetCoupon = DollarRoll@NetCoupon,
      OriginalTerm = DollarRoll@OriginalTerm,
      RemainingTerm = DollarRoll@RemainingTerm,
      OrigBalance = DollarRoll@OrigBalance,
      CurrentBalance = DollarRoll@CurrentBalance,
      Price = DollarRoll@Price,
      PrincipalProceeds = DollarRoll@PrincipalProceeds,
      Accrued = DollarRoll@Accrued,
      TotalProceeds = DollarRoll@TotalProceeds,
      DaysInterest = DollarRoll@DaysInterest,
      ReinvestmentIncome = DollarRoll@ReinvestmentIncome,
      ScheduledPrin = DollarRoll@ScheduledPrin,
      PrepaidPrin = DollarRoll@PrepaidPrin,
      PassThroughInterest = DollarRoll@PassThroughInterest,
      FutureValueHold = DollarRoll@FutureValueHold,
      RemainingBalance = DollarRoll@RemainingBalance,
      FuturePrincipalProceeds = DollarRoll@FuturePrincipalProceeds,
      FwdAccrued = DollarRoll@FwdAccrued,
      Drop = DollarRoll@Drop,
      FwdPrice = DollarRoll@FwdPrice,
      FinanceRate = DollarRoll@FinanceRate,
      ReinvestmentRate = DollarRoll@ReinvestmentRate,
      HoldorRoll = DollarRoll@HoldorRoll,
      Advantage = DollarRoll@Advantage,
      FutureValueRoll = DollarRoll@FutureValueRoll,
      DiscValueofCarry = DollarRoll@DiscValueofCarry,
      FutureValuePrinCarry = DollarRoll@FutureValuePrinCarry,
      TotalFutureValue = DollarRoll@TotalFutureValue,
      DropImpliedValue = DollarRoll@DropImpliedValue)
      }
  
    setGeneric("DollarRollAnalytics", function(bond.id = "character", 
                                               original.bal= numeric(), 
                                               price = numeric(), 
                                               drop = numeric(), 
                                               trade.date = "character", 
                                               settlement.date = "character", 
                                               fwd.settlement.date = "character", 
                                               reinvestment.rate = numeric(),  
                                               finance.rate = numeric(), 
                                               method = "ns", 
                                               PrepaymentAssumption = "character", 
                                               ...,
                                               begin.cpr = numeric(), 
                                               end.cpr = numeric(), 
                                               seasoning.period = numeric(), 
                                               CPR = numeric())
               {standardGeneric("DollarRollAnalytics")})
