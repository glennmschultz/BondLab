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
          signature("DollarRoll"),
          function(.Object,
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
            callNextMethod(.Object,....)
          })
# ---------------- This function is the dollar roll analysis ---------------------------
# ---------------- Currently the function calcualtes the 1 month roll ------------------
# ---------------- Upgrade the bond basis function for actual/actual day count ---------  

#' Dollar Roll Calculates the one-month Dollar Roll
#' 
#' One-month dollar roll analysis.  This function is called by Dollar Roll Analytics
#' @param bond.id A character string the cusip or id of the MBS
#' @param price A numeric value the price of the MBS
#' @param drop A numeric value the one-month drop.
#' @param original.bal A numeric value the original balance of the MBS pool
#' @param settlement.date A character string the current settlement date
#' @param fwd.settlement.date A character string the forward settlement date
#' @param reinvestment.rate A numeric value the investor's reinvestment rate
#' @param finance.rate  A numeric value the investor's alternative finance or repo rate
#' @param MortgageCashFlow A character string the name of a MortgageCashFlow Object
#' @export DollarRoll
DollarRoll <- function(bond.id = "character", 
                       price = numeric(), 
                       drop = numeric(), 
                       original.bal = numeric(), 
                       settlement.date = "character", 
                       fwd.settlement.date = "character", 
                       reinvestment.rate = numeric(), 
                       finance.rate = numeric(),
                       MortgageCashFlow = "character") {
  
  #need to error trap these inputs
  #reinvestment rate, drop
  
  
  #Error Trap the user's price input
  if(price <= 1) {price = price} else {price = price/100}
  if(price <= 0) stop("No valid bond price")
  
  #Error Trap the user's drop input
  #if(drop < 1/32) {drop = drop} else {drop = drop/100}
  
  #Here upgrade the bond basis function to include actual day count
  settlement.date = as.Date(settlement.date, "%m-%d-%Y")
  fwd.settlement.date = as.Date(fwd.settlement.date, "%m-%d-%Y")
  reinvestment.days = as.numeric(difftime(fwd.settlement.date, settlement.date, units = "days"))
  
  Factor = bond.id@MBSFactor
  CurrentBal = as.numeric(original.bal * Factor)
  BeginningMarketValue = original.bal * Factor * price
  
  IssueDate = as.Date(bond.id@IssueDate)
  DatedDate = as.Date(bond.id@DatedDate)
  Maturity = as.Date(bond.id@Maturity)
  Frequency = as.numeric(bond.id@Frequency)
  Coupon = as.numeric(bond.id@Coupon)
  
  FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
  LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  RemainingTerm = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/days.in.month)
  FwdPrice = price - (drop/100)
  reinvestment.rate = reinvestment.rate 
  
  #Dollar Roll Hold versus Roll Analysis..
  #Dollar Roll Proceeds
  Accrued = MortgageCashFlow@Accrued
  TotalProceeds = BeginningMarketValue + Accrued
  ReinvestmentIncome = as.numeric(TotalProceeds * reinvestment.rate * (reinvestment.days/360))
  TotalRollProceeds = TotalProceeds + ReinvestmentIncome
  
  #Hold Proceeds - Calculate the value of holding the MBS
  ScheduledPrin = as.numeric(MortgageCashFlow@ScheduledPrin[1])
  PrepaidPrin = as.numeric(MortgageCashFlow@PrepaidPrin[1])
  PassThroughInterest = as.numeric(MortgageCashFlow@PassThroughInterest[1])
  RemainingBalance = as.numeric(MortgageCashFlow@EndingBal[1])
  
  #Forward settlement and payment dates to compute the value of holding versus rolling
  #Roll Settlement Dates forward by the roll months.  For example one month roll, two months
  #Default value is one month
  
  # Payment Dates work with the mortgage cashflow payment dates.
  # This should be a variable in the function allowing for two-, and three- month rolls
  FwdNextPmtDate = as.Date(MortgageCashFlow@PmtDate[2], "%Y-%m-%d")
  FwdLastPmtDate = LastPmtDate
  days.to.nextpmt = BondBasisConversion(issue.date = IssueDate, 
                                        start.date = DatedDate, 
                                        end.date = Maturity,
                                        settlement.date = fwd.settlement.date, 
                                        lastpmt.date = FwdLastPmtDate, 
                                        nextpmt.date = FwdNextPmtDate)
  
  days.to.nextpmt = days.to.nextpmt * 360
  days.between.pmtdate = ((12/Frequency)/12) * 360
  days.of.accrued = (days.to.nextpmt - days.between.pmtdate) 
  
  FutureValueofPmts = ScheduledPrin + PrepaidPrin + PassThroughInterest
  FuturePrincipalProceeds = RemainingBalance * FwdPrice 
  FwdAccrued = (days.of.accrued/days.between.pmtdate) * as.numeric(MortgageCashFlow@PassThroughInterest[2])
  FutureValueHold = FutureValueofPmts + FuturePrincipalProceeds + FwdAccrued
  
  #Compute the Roll Economics....
  #Hold or Roll Analysis and Economics of Trade  
  if (TotalRollProceeds > FutureValueHold) 
  HoldorRoll = as.character("Roll") else HoldorRoll = as.character("Hold")  
  Advantage = as.numeric(abs(TotalRollProceeds - FutureValueHold))
  
  #Compute the days between the roll settlment date and the payment date
  #This is to derive the discounted value of the carry between the settlement date and the payment date
  #The discount rate is applied to the Future Value of the payments and is the Discounted Value of the Carry
  MBSPmtDate = as.Date(MortgageCashFlow@PmtDate[1], "%Y-%m-%d")
  settlement.day.diff = as.integer(difftime(MBSPmtDate, fwd.settlement.date, units = "days"))
  DiscValueofCarry = FutureValueofPmts * (1/((1 +finance.rate) ^ (settlement.day.diff/360)))
  
  FutureValuePrinCarry = (RemainingBalance * price) + FwdAccrued + DiscValueofCarry
  FinanceCost = as.numeric(TotalProceeds * finance.rate * (reinvestment.days/361))
  TotalFutureValue = FutureValuePrinCarry - FinanceCost
  
  DropImpliedValue = ((TotalFutureValue - TotalProceeds)/RemainingBalance) * 32
  
  new("DollarRoll",
      # -- MBS price and settlment information
      SettlementDate = as.character(settlement.date),
      FwdSettlementDate = as.character(fwd.settlement.date),
      Price = price * 100,
      Drop = drop,
      FwdPrice = FwdPrice * 100,
      # -- MBS information
      GrossCoupon = bond.id@GWac,
      NetCoupon = bond.id@Coupon,
      OriginalTerm = bond.id@AmortizationTerm,
      RemainingTerm = RemainingTerm,
      OrigBalance = original.bal,
      CurrentBalance = CurrentBal,
      # -- Settlement information
      PrincipalProceeds = BeginningMarketValue,
      Accrued = Accrued,
      TotalProceeds = TotalProceeds,
      DaysInterest = reinvestment.days,
      ReinvestmentIncome = ReinvestmentIncome,
      # -- Determine MBS hold information
      ScheduledPrin = ScheduledPrin,
      PrepaidPrin = PrepaidPrin,
      PassThroughInterest = PassThroughInterest,
      FutureValueHold = FutureValueHold,
      RemainingBalance = RemainingBalance,
      FuturePrincipalProceeds = FuturePrincipalProceeds,
      FwdAccrued = FwdAccrued,
      # -- Compute Investor Financing Rates
      FinanceRate = finance.rate,
      ReinvestmentRate = reinvestment.rate,
      FutureValueRoll = TotalRollProceeds,
      FutureValuePrinCarry = FutureValuePrinCarry,
      DiscValueofCarry = DiscValueofCarry,
      # -- Compute Roll analysis
      HoldorRoll = HoldorRoll,
      Advantage = Advantage,
      TotalFutureValue = TotalFutureValue,
      DropImpliedValue = DropImpliedValue)}

setGeneric("DollarRoll", function(bond.id = "character", 
                                  price = numeric(), 
                                  drop = numeric(), 
                                  original.bal = numeric(), 
                                  settlement.date = "character", 
                                  fwd.settlement.date = "character", 
                                  reinvestment.rate = numeric(), 
                                  finance.rate = numeric(),
                                  MortgageCashFlow = "character") 
{standardGeneric("DollarRoll")})