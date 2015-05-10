# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics"

  setMethod("initialize",
            signature("REMICAnalytics"),
            function(.Object,
                     DealName = "character",
                     TrancheNumber = "character",
                     TrancheName = "character",
                     TranchePrincipal = "character",
                     TrancheInterest = "character",
                     TranchePrincipalDesc = "character",
                     TrancheInterestDesc = "character",
                     Cusip = "character",
                     TrancheOrigBal = numeric(),
                     TrancheDatedDate = "character",
                     TrancheFirstPmtDate = "character",
                     TrancheLastPmtDate = "character",
                     TrancheNextPmtDate = "character",
                     TrancheCoupon = numeric(),
                     Delay = numeric(),
                     PrinPmtFrequency = numeric(),
                     InterestPmtFrequency = numeric(),
                     FloaterIndex = "character",
                     FloaterMargin = numeric(),
                     FloaterCap = numeric(),
                     FloaterFloor = numeric(),
                     FloaterFormula = "function",
                     PacLowBand = numeric(),
                     PacHighBand = numeric(),
                     Group = numeric(),
                     Schedule = "logical",
                     Fixed = "logical",
                     Price = numeric(),
                     PrincipalProceeds = numeric(),
                     Accrued = numeric(),
                     YieldToMaturity = numeric(),
                     WAL = numeric(),
                     ModDuration = numeric(),
                     Convexity = numeric(),
                     Period = numeric(),
                     PmtDate = "character",
                     TimePeriod = "numeric",
                     Interest = numeric(),
                     Principal = numeric(),
                     TotalCashFlow = numeric(),
                     SpotSpread = numeric(),
                     EffDuration = numeric(),
                     EffConvexity = numeric(),
                     KeyRateTenor = numeric(),
                     KeyRateDuration = numeric(),
                     KeyRateConvexity = numeric())
                  {
                    .Object@DealName = DealName
                    .Object@TrancheNumber = TrancheNumber
                    .Object@TrancheName = TrancheName
                    .Object@TranchePrincipal = TranchePrincipal
                    .Object@TrancheInterest = TrancheInterest
                    .Object@TranchePrincipalDesc = TranchePrincipalDesc
                    .Object@TrancheInterestDesc = TrancheInterestDesc
                    .Object@Cusip = Cusip
                    .Object@TrancheOrigBal = TrancheOrigBal
                    .Object@TrancheDatedDate = TrancheDatedDate
                    .Object@TrancheFirstPmtDate = TrancheFirstPmtDate
                    .Object@TrancheLastPmtDate = TrancheLastPmtDate
                    .Object@TrancheNextPmtDate = TrancheNextPmtDate
                    .Object@TrancheCoupon = TrancheCoupon
                    .Object@Delay = Delay
                    .Object@PrinPmtFrequency = PrinPmtFrequency
                    .Object@InterestPmtFrequency = InterestPmtFrequency
                    .Object@FloaterIndex = FloaterIndex
                    .Object@FloaterMargin = FloaterMargin
                    .Object@FloaterCap = FloaterCap
                    .Object@FloaterFloor = FloaterFloor
                    .Object@FloaterFormula = FloaterFormula
                    .Object@PacLowBand = PacLowBand
                    .Object@PacHighBand = PacHighBand
                    .Object@Group = Group
                    .Object@Price = Price
                    .Object@PrincipalProceeds = PrincipalProceeds
                    .Object@Accrued = Accrued
                    .Object@YieldToMaturity = YieldToMaturity
                    .Object@WAL = WAL
                    .Object@ModDuration = ModDuration
                    .Object@Convexity = Convexity
                    .Object@Period = Period
                    .Object@PmtDate = PmtDate
                    .Object@TimePeriod = TimePeriod
                    .Object@Interest = Interest
                    .Object@Principal = Principal
                    .Object@TotalCashFlow = TotalCashFlow
                    .Object@SpotSpread = SpotSpread
                    .Object@EffDuration = EffDuration
                    .Object@EffConvexity = EffConvexity
                    .Object@KeyRateTenor = KeyRateTenor
                    .Object@KeyRateDuration = KeyRateDuration
                    .Object@KeyRateConvexity = KeyRateConvexity
                    
                    return(.Object)
                    callNextMethod(.Object,...)
                  })
#' The REMIC Analytics engine the constructor function for REMIC Analytics
#' 
#' Cash flow analysis and risk metrics of REMIC Tranches including spot spread analysis
#' effective duration, effective convexity, Key rate duration analysis
#' @param bond.id A character string the bond.id or cusip number
#' @param trade.date A character string the trade date
#' @param settlement.date A character string the settlement date
#' @param method A character string the method used to fit the term strucutre
#' @param collateral.price A numeric value the price of the underlying collateral
#' @param tranche.price A numeric value the price of the tranche
#' @param PrepaymentAssumption A character string defining the prepayment assumption used
#' @param begin.cpr optional, a numeric value used when applying a PPC assumption
#' @param ... Optional values when CPR and PPC are input
#' @param end.cpr optional, a numeric value used when applying a PPC assumption
#' @param seasoning.period optional, a numeric value used when applying a PPC assumption
#' @param CPR optional, a numeric value used when applying a CPR assumption
#' @examples 
#' \dontrun{REMICAnalytics(bond.id = "BondLabSMBSIO", trade.date = "01-10-2013",
#' settlement.date = "01-13-2013", method = "ns", collateral.price = 105.75,
#' tranche.price = 25, PrepaymentAssumption = "CPR", CPR = 6)}
#' @export REMICAnalytics
  REMICAnalytics <- function(bond.id = "character", 
                           trade.date = "character",
                           settlement.date = "character",
                           method = "character",
                           collateral.price = numeric(),
                           tranche.price = numeric(),
                           PrepaymentAssumption = "character",
                           ...,
                           begin.cpr = numeric(),
                           end.cpr = numeric(),
                           seasoning.period = numeric(),
                           CPR = numeric()){
  
  #connect to rates data
  REMIC.Tranche <- MBS(MBS.id = bond.id)
  
  # ---- connect to rates data folder
  rates.data <- Rates(trade.date = trade.date)
  
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  MortgageCashFlow <-  REMICCashFlow(bond.id = bond.id, 
                                     trade.date = trade.date,
                                     settlement.date = settlement.date,
                                     collateral.price = collateral.price,
                                     tranche.price = tranche.price,
                                     PrepaymentAssumption = PrepaymentAssumption,
                                     ...,
                                     begin.cpr = begin.cpr,
                                     end.cpr = end.cpr,
                                     seasoning.period = seasoning.period,
                                     CPR = CPR,
                                     KeyRateTermStrucuture = NULL)
  
  Rate.Delta = .25
  #The fifth step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  REMICTermStructure <- REMICTermStructure(bond.id = bond.id, 
                                            original.bal = REMIC.Tranche@TrancheOrigBal, 
                                            Rate.Delta = Rate.Delta, 
                                            TermStructure = TermStructure, 
                                            settlement.date = settlement.date,
                                            trade.date = trade.date,
                                            collateral.price = collateral.price,
                                            tranche.price = tranche.price, 
                                            cashflow = MortgageCashFlow)
  
  new("REMICAnalytics",
      DealName = REMIC.Tranche@DealName,
      TrancheNumber = REMIC.Tranche@TrancheNumber,
      TrancheName = REMIC.Tranche@TrancheName,
      TranchePrincipal = REMIC.Tranche@TranchePrincipal,
      TrancheInterest = REMIC.Tranche@TrancheInterest,
      TranchePrincipalDesc = REMIC.Tranche@TranchePrincipalDesc,
      TrancheInterestDesc = REMIC.Tranche@TrancheInterestDesc,
      Cusip = REMIC.Tranche@Cusip,
      TrancheOrigBal = REMIC.Tranche@TrancheOrigBal,
      TrancheDatedDate = REMIC.Tranche@TrancheDatedDate,
      TrancheFirstPmtDate = REMIC.Tranche@TrancheFirstPmtDate,
      TrancheLastPmtDate = REMIC.Tranche@TrancheLastPmtDate,
      TrancheNextPmtDate = REMIC.Tranche@TrancheNextPmtDate,
      TrancheCoupon = REMIC.Tranche@TrancheCoupon,
      Delay = REMIC.Tranche@Delay,
      PrinPmtFrequency = REMIC.Tranche@PrinPmtFrequency,
      InterestPmtFrequency = REMIC.Tranche@InterestPmtFrequency,
      FloaterIndex = REMIC.Tranche@FloaterIndex,
      FloaterMargin = REMIC.Tranche@FloaterMargin,
      FloaterCap = REMIC.Tranche@FloaterCap,
      FloaterFloor = REMIC.Tranche@FloaterFloor,
      FloaterFormula = REMIC.Tranche@FloaterFormula,
      PacLowBand = REMIC.Tranche@PacLowBand,
      PacHighBand = REMIC.Tranche@PacHighBand,
      Group = REMIC.Tranche@Group,
      Schedule = REMIC.Tranche@Schedule,
      Fixed = REMIC.Tranche@Fixed,
      Price = MortgageCashFlow@Price,
      PrincipalProceeds = MortgageCashFlow@PrincipalProceeds,
      Accrued = MortgageCashFlow@Accrued,
      YieldToMaturity = MortgageCashFlow@YieldToMaturity,
      WAL = MortgageCashFlow@WAL,
      ModDuration = MortgageCashFlow@ModDuration,
      Convexity = MortgageCashFlow@Convexity,
      Period = MortgageCashFlow@Period,
      PmtDate = MortgageCashFlow@PmtDate,
      TimePeriod = MortgageCashFlow@TimePeriod, 
      Interest = MortgageCashFlow@Interest,
      Principal = MortgageCashFlow@Principal,
      TotalCashFlow = MortgageCashFlow@TotalCashFlow,
      SpotSpread = REMICTermStructure@SpotSpread,
      EffDuration = REMICTermStructure@EffDuration,
      EffConvexity = REMICTermStructure@EffConvexity,
      KeyRateTenor = REMICTermStructure@KeyRateTenor,
      KeyRateDuration = REMICTermStructure@KeyRateDuration,
      KeyRateConvexity = REMICTermStructure@KeyRateConvexity
      )  
}
  
setGeneric("REMICAnalytics", function(bond.id = "character", 
                                      trade.date = "character",
                                      settlement.date = "character",
                                      method = "character",
                                      collateral.price = numeric(),
                                      tranche.price = numeric(),
                                      PrepaymentAssumption = "character",
                                      ...,
                                      begin.cpr = numeric(),
                                      end.cpr = numeric(),
                                      seasoning.period = numeric(),
                                      CPR = numeric())
           {standardGeneric("REMICAnalytics")})  