  
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.

  #' @include ScenarioConstructor.R MortgageCashFlow.R TermStructure.R 
  #' @include PrepaymentModel.R MortgageKeyRate.R ModelToCPR.R
  NULL
  
  
  #' @title MortgageReturn class
  #' @family Mortgage Scenario Analysis
  #' @description 
  #' The class MortgageReturn holds the results of mortgage return analysis.
  #' MortgageReturn is the sum of coupon income, principal received, reinvestment
  #' income and price appreciation or depreciation.  In addition, most investors
  #' also track horizon current balance. 
  #' @slot CouponIncome A numeric value the coupon income received over the 
  #' investment horizon
  #' @slot ScheduledPrinReceived A numeric value the scheduled principal 
  #' received over the investment horizon
  #' @slot PrepaidPrinReceived A numeric value the prepaid principal received
  #' over the investment horizon
  #' @slot ReinvestmentIncome A numeric value the reivestment income received 
  #' over the investment horizon
  #' @slot HorizonCurrBal A numeric value the current balance at the end of the
  #' investment horizon
  #' @slot HorizonPrice A numeric the price at the end of the horizon
  #' @slot HorizonReturn A numeric value the horizon total return
  #' @slot HorizonMos A numeric value the number of months to 
  #' the scenario horizon date
  #' @exportClass MortgageReturn
  setClass("MortgageReturn",
           representation(
             CouponIncome = "numeric",
             ScheduledPrinReceived = "numeric",
             PrepaidPrinReceived = "numeric",
             ReinvestmentIncome = "numeric",
             HorizonCurrBal = "numeric",
             HorizonPrice = "numeric",
             HorizonReturn = "numeric",
             HorizonMos = "numeric"))
  
  #' @title CouponIncome generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object An object of the type MtgScenario
  #' @export
  setGeneric("CouponIncome", function(object)
  {standardGeneric("CouponIncome")})
  
  #' @title ScheduledPrincipalReceived generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object An object of the type MtgScenario
  #' @export
  setGeneric("ScheduledPrinReceived", function(object)
  {standardGeneric("ScheduledPrinReceived")})
  
  #' @title PrepaidPrinReceived generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object An object of the type MtgScenario
  #' @export
  setGeneric("PrepaidPrinReceived", function(object)
  {standardGeneric("PrepaidPrinReceived")})
  
  #' @title ReinvestmentIncome generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object an object of the type MtgScenario
  #' @export
  setGeneric("ReinvestmentIncome", function(object)
  {standardGeneric("ReinvestmentIncome")})
  
  #' @title HorizonCurrBal generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object An object of the type MtgScenario
  #' @export
  setGeneric("HorizonCurrBal", function(object)
  {standardGeneric("HorizonCurrBal")})
  
  #' @title HorizonPrice generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object an S4 object of the type MtgScenario
  #' @export
  setGeneric("HorizonPrice", function(object)
  {standardGeneric("HorizonPrice")})
  
  #' @title HorizonPrice generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object An object of the type MtgScenario
  #' @export
  setGeneric("HorizonReturn", function(object)
  {standardGeneric("HorizonReturn")})
  
  #' @title HorizonMos generic
  #' @family Scenario Analysis
  #' @description A generic function for method dispatch
  #' @param object An object of the type MtgScenario
  #' @export
  setGeneric("HorizonMos", function(object)
  {standardGeneric("HorizonMos")})
  
  setMethod("initialize",
            signature("MortgageReturn"),
            function(.Object,
                     CouponIncome = numeric(),
                     ScheduledPrinReceived = numeric(),
                     PrepaidPrinReceived = numeric(),
                     ReinvestmentIncome = numeric(),
                     HorizonCurrBal = numeric(),
                     HorizonPrice = numeric(),
                     HorizonReturn = numeric(),
                     HorizonMos = numeric(),
                     ...)
              {
              callNextMethod(.Object,
                             CouponIncome = CouponIncome,
                             ScheduledPrinReceived = ScheduledPrinReceived,
                             PrepaidPrinReceived = PrepaidPrinReceived,
                             ReinvestmentIncome =ReinvestmentIncome,
                             HorizonCurrBal = HorizonCurrBal,
                             HorizonPrice = HorizonPrice,
                             HorizonReturn = HorizonReturn,
                             HorizonMos = HorizonMos,
                             ...)
            })

  #' @title CouponIncome method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get the  \strong{CouponIncome} paid to the 
  #' to the investor over the scenrio horizon.  CouponIncome is reported as the 
  #' sum of the coupon income received by the investor.
  #' @param object A class of the type MortgageReturn
  #' @exportMethod CouponIncome
  setMethod("CouponIncome", signature("MortgageReturn"),
            function(object){object@CouponIncome})
  
  #' @title ScheduledPrinReceived method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get the \strong{ScheduledPrinRecieved} paid to the
  #' investor over the scenario horizon.  ScheduledPrinReceived is reported as 
  #' the sum of the sceduled principal received over the horizon.
  #' @param object An S4 class of type MortgageReturn
  #' @exportMethod ScheduledPrinReceived
  setMethod("ScheduledPrinReceived", signature("MortgageReturn"),
            function(object){object@ScheduledPrinReceived})
  
  #' @title PrepaidPrinReceived method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get \strong{PrepaidPrinReceived} paid to the 
  #' investor over the scenario horizon.  PrepaidPrinReceived is reported as
  #' the sum of the prepaid principal received over the horizon.
  #' @param object An S4 class of type MortgageReturn
  #' @exportMethod PrepaidPrinReceived
  setMethod("PrepaidPrinReceived", signature("MortgageReturn"),
            function(object){object@PrepaidPrinReceived})
  
  #' @title ReinvestmentIncome method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get \strong{ReinvestmentIncome} paid to the 
  #' investor over the scenario horizon.  ReinvesmentIncome is reported as
  #' the sum of the reinvestment income received over the horizon.
  #' @param object An S4 class of the type MortgageReturn
  #' @exportMethod ReinvestmentIncome
  setMethod("ReinvestmentIncome", signature("MortgageReturn"),
            function(object){object@ReinvestmentIncome})
  
  #' @title HorizonCurrBal method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get \strong{HorizonCurrBal} the mortgage current
  #' balance after principal paydown.
  #' @param object An S4 class of the type MortgageReturn
  #' @exportMethod HorizonCurrBal
  setMethod("HorizonCurrBal", signature("MortgageReturn"),
            function(object){object@HorizonCurrBal})
  
  #' @title HorizonPrice method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get \strong{HorizonPrice} used to calculate
  #' mortgage total return.
  #' @param object An S4 class of the type MortgageReturn
  #' @exportMethod HorizonPrice
  setMethod("HorizonPrice", signature("MortgageReturn"),
            function(object){object@HorizonPrice})
  
  #' @title HorizonReturn method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get \strong{HorizonReturn}
  #' @param object An S4 class of the type MortgageReturn
  #' @exportMethod HorizonReturn
  setMethod("HorizonReturn", signature("MortgageReturn"),
            function(object){object@HorizonReturn})
  
  #' @title HorizonMos method, class MortgageReturn
  #' @family Mortgage Scenario Analysis
  #' @description A method to get \strong{HorizonMos}
  #' @param object An S4 class of the type MortgageReturn
  #' @exportMethod HorizonMos
  setMethod("HorizonMos", signature("MortgageReturn"),
            function(object){object@HorizonMos})
  
  #' @title MortgageScenario class
  #' @family Mortgage Scenario Analysis
  #' @description The call MortgageScenario contains the following super classes:
  #' TermStructure, PrepaymentAssumption, MortgageCashFlow, MortgageTermStructure
  #' MortgageReturn, ModelToCPR. CurveSpreads, and Scenario.  MortgageScenario
  #' inherits the  getters and setter of each of the above super classes.
  #' @exportClass MortgageScenario    
  setClass("MortgageScenario",
           representation(),
           contains = c("TermStructure",
                        "PrepaymentModel",
                        "MortgageCashFlow",
                        "MortgageTermStructure",
                        "MortgageReturn",
                        "ModelToCPR",
                        "CurveSpreads",
                        "Scenario"))

  setGeneric("MortgageScenario", function(bond.id ="character",
                                          settlement.date = "character",
                                          rates.data = "character",
                                          price = "character", 
                                          original.bal = numeric(),
                                          scenario = "character",
                                          horizon.months = numeric(),
                                          method = "character",
                                          prepayment = "character",
                                          ...,
                                          horizon.spot.spread = NULL,
                                          horizon.nominal.spread = NULL,
                                          horizon.OAS = NULL,
                                          horizon.price = NULL,
                                          begin.cpr = numeric(),
                                          end.cpr = numeric(),
                                          seasoning.period = numeric(),
                                          CPR = numeric())
             {standardGeneric("MortgageScenario")})
  
  setMethod("initialize",
            signature("MortgageScenario"),
            function(.Object,
                     ...)
            {callNextMethod(.Object,
                             ...)
            })
  
  #---------------------------------------------------------
  # Scenario Total Return Analysis
  # Calls the bond.id and applies the scenario
  #---------------------------------------------------------
  
  #' Mortgage Scenario Analysis
  #' 
  #' A function to compute the total return of mortgage pass-throughs MBS
  #' @param bond.id A character string referencing an object of type MBSDetails
  #' @param settlement.date A charcter string the settlement date
  #' @param rates.data A character string the trade date mm-dd-yyyy
  #' @param price A character string in decimal equivalent (.) or 32nds (-)
  #' @param original.bal A numeric value the price
  #' @param scenario A character string the scenario
  #' @param horizon.months A numeric value the time horizon
  #' @param prepayment A character string the prepayment assumption
  #' @param ... Optional values when PSA or CPR is used or Yield Curve
  #' is used
  #' @param method A character string the method used to fit the 
  #' term structure
  #' @param horizon.spot.spread A numeric value the horizon zero volatility 
  #' spread
  #' @param horizon.nominal.spread A numeric value the horizon spread
  #' @param horizon.OAS A numeric value the horizon option adjusted spread
  #' (not currently implemented)
  #' @param horizon.price A numeric value the horizon price in decimal form
  #' @param begin.cpr A numeric value the beginning CPR value
  #' @param end.cpr A numeric value the ending CPR value
  #' @param seasoning.period A numeric value the length of the 
  #' seasoning ramp
  #' @param CPR A numeric value the CPR speed
  #' @export
  MortgageScenario <- function(bond.id ="character",
                          settlement.date = "character",
                          rates.data = "character",
                          price = "character", 
                          original.bal = numeric(),
                          scenario = "character",
                          horizon.months = numeric(),
                          method = "character",
                          prepayment = "character",
                          ...,
                          horizon.spot.spread = NULL,
                          horizon.nominal.spread = NULL,
                          horizon.OAS = NULL,
                          horizon.price = NULL,
                          begin.cpr = numeric(),
                          end.cpr = numeric(),
                          seasoning.period = numeric(),
                          CPR = numeric()) { 
    
   
    # Mortgage Scenario analysis is done in two steps
    # The first is calculated the expected cash-flows received over the 
    # investment horizon The second is to "roll" the pass through security 
    # forward and price the expected future cash-flows
    
    # logical declarations of scenario analysis are horzion pricing methods
    # and horizon term structure assumption used shift the sopt curve or shift
    # the coupon curve and refit the term structure model
    
    if(is.null(horizon.spot.spread) != TRUE) {
      horizon.price.type <- "spot"
    } else if(is.null(horizon.nominal.spread) != TRUE) {
      horizon.price.type <- "nominal"  
    } else if(is.null(horizon.OAS) != TRUE) {
      horizon.price.type <- "oas"  
    } else {
      horizon.price.type <- "price"
    }
    
    bond.id <- bond.id
    rates.data <- rates.data
    MortgageRate <- MtgRate()
    ModelTune <- ModelTune(bond.id = bond.id)
    Burnout = BurnOut(bond.id)
    Scenario <- ScenarioCall(Scenario = scenario)

    #set rates shift (immediate) for term structure fit
    ShiftCurve <- rates.data
    ShiftCurve[1,2:length(ShiftCurve)] <- 
      ScenarioFormula(Scenario)(rates.data[1,2:length(ShiftCurve)], 
                                Shiftbps = Shiftbps(Scenario))

    # Set horizon curve and settlment date for horizon ending value analysis
    # This curve is used to fit the horizon term structure when the scenario is
    # not based on shift of the spot rate curve.
    
    HorizonCurve <- rates.data
    HorizonCurve[1,1] <- as.character(
      as.Date(HorizonCurve[1,1]) %m+% months(horizon.months))
    
    HorizonCurve[1,2:length(HorizonCurve)] <- 
      ScenarioFormula(Scenario)(rates.data[1,2:length(HorizonCurve)], 
                                Shiftbps = Shiftbps(Scenario))
    HorizonSettlement <- as.Date(
      settlement.date, format = "%m-%d-%Y") %m+% months(horizon.months)
    
    Price <- PriceTypes(Price = price)
  
    # fit the term structure and calcualate cashflows to compute the 
    # following Yield, WAL, Curve Spreads, KeyRate Durations, etc

    TermStructure <- TermStructure(
      rates.data = ShiftCurve,
      method = method)
   
    Prepayment <- PrepaymentModel(
      bond.id = bond.id,
      MortgageRate = MortgageRate,
      TermStructure = TermStructure,
      PrepaymentAssumption = prepayment,
      ModelTune = ModelTune,
      Severity = 0,
      Burnout = Burnout,
      begin.cpr = begin.cpr,
      end.cpr = end.cpr,
      seasoning.period = seasoning.period,
      CPR = CPR)
    
    MortgageCashFlow <- MortgageCashFlow(
      bond.id = bond.id,
      original.bal = original.bal,
      settlement.date = settlement.date,
      price = PriceDecimalString(Price),
      PrepaymentAssumption = Prepayment)

    proceeds <- Accrued(MortgageCashFlow) + 
      (original.bal * MBSFactor(bond.id) * PriceBasis(Price))
    principal <- original.bal * MBSFactor(bond.id)
    
    # Compute the CurvesSpreads based on the user price and prepayment vector
    # given the user's scenario interest rate shift
    CurveSpread <- CurveSpreads(rates.data = rates.data,
                                CashFlow = MortgageCashFlow,
                                TermStructure = TermStructure,
                                proceeds = proceeds)
    
    # Compute life CPR using the base case term structure fit
    LifeCPR <- ModelToCPR(
      bond.id = bond.id,
      TermStructure = TermStructure,
      MortgageRate = MortgageRate,
      ModelTune = ModelTune,
      Burnout = Burnout,
      original.bal = original.bal,
      settlement.date = settlement.date,
      price = PriceDecimalString(Price),
      yield = YieldToMaturity(MortgageCashFlow)/yield.basis
    )
    
    # compute the key rate duration, effective duration, and convexity 
    # given the base case structure pricing. 
    MortgageTermStructure <- MtgTermStructure(
      bond.id = bond.id,
      original.bal = original.bal,
      Rate.Delta = rate.delta,
      TermStructure = TermStructure,
      settlement.date = settlement.date,
      principal = principal,
      price = PriceDecimalString(Price),
      cashflow = MortgageCashFlow)
    
    
    # This section begins the  horizon mortgage pass-through analysis. 
    # Horizon curve can be calculated by either shifting the coupon curve or
    # and refitting the curve or it can be calculated by shifting the spot rate
    # curve.  Senarios with ending with (s) indicate the user wishes to shift
    # the spot rate curve.
    
    if(grepl("s",scenario) == TRUE){
      # initialize term structure object and assign shift vlaue to the 
      # spot rate curve based on the TermStructure object above
      
      HorizonTermStructure <- new(
        "TermStructure",
        TradeDate = as.character(
          as.Date(rates.data[1,1]) %m+% months(horizon.months)),
        Period = numeric(),
        Date = "character",
        SpotRate = numeric(),
        ForwardRate = numeric(),
        TwoYearFwd = numeric(),
        TenYearFwd = numeric())
      
      Period(HorizonTermStructure) <- Period(TermStructure)
      ForwardDate(HorizonTermStructure) <- as.character(
        as.Date(ForwardDate(TermStructure)) %m+% months(horizon.months))
      ForwardRate(HorizonTermStructure) <-ForwardRate(TermStructure)
      SpotRate(HorizonTermStructure) <- SpotRate(TermStructure)
      TwoYearForward(HorizonTermStructure) <- TwoYearForward(TermStructure)
      TenYearForward(HorizonTermStructure) <- TenYearForward(TermStructure)
    } else {
    HorizonTermStructure <- TermStructure(
      rates.data = HorizonCurve,
      method = "ns")
    } # End of if logic for term structure method
    
    # replace this code with getters and setters.  This will reduce the disk IO
    # and should speed up the calculation.  In addition it will eliminate the need
    # the Temp_BondData folder
    #ForwardPassThrough(
    #  bond.id = bond.id,
    #  original.bal = original.bal,
    #  projected.cashflow = MortgageCashFlow,
    #  horizon.months = horizon.months)

    #HorizonMBS <- HorizonMBS()
    
    # This section of code rolls the MBS pass-though forward in time updating
    # factor, current balance, lastpaymentdate, nextpaymentdate, wam and wala the 
    # first step is to assign bond.id to HorizonMBS object
    HorizonMBS <- bond.id
    
    # Use the projected cashflow to determine the current balance outstanding 
    # at the end of horizon
    SchedPrincipal <- ScheduledPrin(MortgageCashFlow)[1:horizon.months]
    PrepaidPrincipal <- PrepaidPrin(MortgageCashFlow)[1:horizon.months]
    DefaultedPrincipal <- DefaultedPrin(MortgageCashFlow)[1:horizon.months]
    TotalPrincipal <- sum(SchedPrincipal) + sum(PrepaidPrincipal) + sum(DefaultedPrincipal)
    
    # Update the LastPmtDate and NextPmtDate to reflect the end of the horizon
    # and compute and update the MBSFactor, CurrentBal, WAM, and WALA
    LastPmtDate(HorizonMBS) <- as.character(format(
      as.Date(LastPmtDate(bond.id), 
              format = "%m-%d-%Y") %m+% months(horizon.months), "%m-%d-%Y"))
    
    NextPmtDate(HorizonMBS) <- as.character(format(
      as.Date(NextPmtDate(bond.id), 
              format = "%m-%d-%Y") %m+% months(horizon.months), "%m-%d-%Y"))
    
    MBSFactor(HorizonMBS) <- ((original.bal * 
                                  MBSFactor(bond.id)) - TotalPrincipal)/original.bal
    
    CurrentBal(HorizonMBS) <- CurrentBal(bond.id) - TotalPrincipal
    WAM(HorizonMBS) <- WAM(bond.id) - horizon.months
    WALA(HorizonMBS) <- WALA(bond.id) + horizon.months
    
    HorizonPrepaymentAssumption <- PrepaymentModel(
      bond.id = HorizonMBS,
      MortgageRate = MortgageRate,
      TermStructure = HorizonTermStructure,
      PrepaymentAssumption = prepayment,
      ModelTune = ModelTune,
      Severity = 0,
      Burnout = Burnout,
      begin.cpr = begin.cpr,
      end.cpr = end.cpr,
      seasoning.period = seasoning.period,
      CPR = CPR)
    
    HorizonCashFlow <- MortgageCashFlow(
      bond.id = HorizonMBS,
      original.bal = original.bal,
      settlement.date = HorizonSettlement,
      price = PriceDecimalString(Price),
      PrepaymentAssumption = HorizonPrepaymentAssumption)
    
    # =========================================================================
    # This section begins the calculation of horizon total return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # ========================================================================
    
    NumberofCashFlow <- as.numeric(length(TotalCashFlow(HorizonCashFlow)))
    reinvestment.rate <- as.numeric(HorizonCurve[1,2])/yield.basis
    
    # =========================================================================
    # Horizon present value of MBS pass through using spot spread, nominal 
    # spread or OAS use switch here to compute the horizon present value based 
    # and on either spot spread, nominal spread, or horizon price. (At this 
    # time there is no OAS to price module) The functions Horizon.Spot.Value, 
    # Horizon.Nominal.Value, and Horizon.Price.Value are used to determine the
    # present value of the remaining cash flows are the horizon date.  
    # The switch function determines which function is called based on 
    # horizon.price.type
    # ========================================================================

  Horizon.Spot.Value <- function(HorizonTermStructure = "character",
                                   HorizonCashFlow = "character",
                                   HorizonSpotSpread = numeric(),
                                   NumberofCashFlow = numeric()){
  DiscountRate <- 1/((1+((SpotRate(HorizonTermStructure)[1:NumberofCashFlow] + 
                           horizon.spot.spread)/monthly.yield.basis)) ^ 
                          (Period(HorizonTermStructure)[1:NumberofCashFlow]))
      
  HorizonPresentValue <- 
    DiscountRate[1:NumberofCashFlow] * TotalCashFlow(HorizonCashFlow)
    PresentValue <- sum(HorizonPresentValue)
    return(PresentValue)}
  
  # Do not replace this with curve spreads as this section of code is used 
  # to compute horizon yield to maturity from nominal spread and interpolated
  # curve.  
  Horizon.Nominal.Value <- function(HorizonCurve = "character",
                           HorizonTermStructure = "character",
                          HorizonCashFlow = "character"){
  InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                            as.numeric(rates.data[2,2:12]),
                            data = data.frame(HorizonCurve))
      
  HorizonYield <- 
      predict(InterpolateCurve, WAL(HorizonCashFlow)) + horizon.nominal.spread
      HorizonYield <- rep(HorizonYield, NumberofCashFlow)
      DiscountRate <- 1/((1 + (HorizonYield/monthly.yield.basis))^
                           (Period(HorizonTermStructure)[1:NumberofCashFlow]))
      HorizonPresentValue <- DiscountRate * TotalCashFlow(HorizonCashFlow)
      PresentValue <- sum(HorizonPresentValue)
      return(PresentValue)}
  
  # Horizon.Price.Value is a function which returns the principal proceeds
  # at the horizon date and is used to calculate PresentValue
  Horizon.Price.Value <- function(HorizonBond = "character",
                                    HorizonPrice = numeric()){
      price.basis = 100
      original.bal * MBSFactor(HorizonBond) * (HorizonPrice/price.basis)
    }
  
  PresentValue <- switch(horizon.price.type,
    "spot" = Horizon.Spot.Value(HorizonTermStructure = HorizonTermStructure,
    HorizonCashFlow = HorizonCashFlow,
    HorizonSpotSpread = horizon.spot.spread,
    NumberofCashFlow = NumberofCashFlow),
    "nominal" = Horizon.Nominal.Value(HorizonCurve = HorizonCurve,
    HorizonTermStructure = HorizonTermStructure,
    HorizonCashFlow = HorizonCashFlow),
    "price" = Horizon.Price.Value(HorizonBond = HorizonMBS,
    HorizonPrice = horizon.price))
    
  HorizonPrice <- if(horizon.price.type == "price"){horizon.price} else {
  (PresentValue / (original.bal * MBSFactor(HorizonMBS))) * price.basis}
  
  
  # Replace this with PriceTypes objects  
  HorizonPrice <- sprintf("%.8f", HorizonPrice)
  
  HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                    original.bal = original.bal,
                    settlement.date = HorizonSettlement,
                    price = HorizonPrice,
                    PrepaymentAssumption = HorizonPrepaymentAssumption)
  
  HorizonProceeds <- ((as.numeric(HorizonPrice)/price.basis * 
                         MBSFactor(HorizonMBS) *
                         original.bal) + Accrued(HorizonCashFlow))

  HorizonSpread <- CurveSpreads(
    rates.data = HorizonCurve,
    CashFlow = HorizonCashFlow,
    TermStructure = HorizonTermStructure,
    proceeds = HorizonProceeds)
  
  # From the beginning cashflow calculation get the cashflow recieved by the 
  # investor.
  CouponIncome <- sum(MortgageCashFlow@PassThroughInterest[1:horizon.months])
  ReceivedCashFlow <- TotalCashFlow(MortgageCashFlow)[1:horizon.months]

  n.period <- 
    as.numeric(difftime(as.Date(PmtDate(MortgageCashFlow)[horizon.months]), 
    as.Date(PmtDate(MortgageCashFlow)[1:horizon.months]), 
    units = "days")/days.in.month)
    
  TerminalValue <- 
    ReceivedCashFlow * ((1 + (reinvestment.rate/months.in.year)) ^ (n.period))
    ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow))

  PrincipalRepaid <- sum(PrepaidPrin(MortgageCashFlow)[1:horizon.months]) + 
  sum(ScheduledPrin(MortgageCashFlow)[1:horizon.months])
    
  HorizonValue <- 
    CouponIncome + 
    ReinvestmentIncome + 
    PrincipalRepaid + 
    PresentValue
  
  HorizonReturn <- (HorizonValue/proceeds)^(months.in.year/horizon.months)
  HorizonReturn <- (HorizonReturn - 1) * yield.basis
    
  new("MortgageScenario",
      Period = Period(MortgageCashFlow),
      PmtDate = PmtDate(MortgageCashFlow),
      TimePeriod = TimePeriod(MortgageCashFlow),
      BeginningBal = BeginningBal(MortgageCashFlow),
      PassThroughInterest = PassThroughInterest(MortgageCashFlow),
      ScheduledPrin = ScheduledPrin(MortgageCashFlow),
      PrepaidPrin = PrepaidPrin(MortgageCashFlow),
      EndingBal = EndingBalance(MortgageCashFlow),
      TotalCashFlow = TotalCashFlow(MortgageCashFlow),
      SpotRate = SpotRate(TermStructure),
      ForwardRate = ForwardRate(TermStructure),
      TwoYearFwd = TwoYearForward(TermStructure),
      TenYearFwd = TenYearForward(TermStructure),
      SMM = SMM(Prepayment),
      CPRLife = CPRLife(LifeCPR),
      BenchMark = BenchMark(HorizonSpread),
      SpreadToBenchmark = SpreadToBenchmark(HorizonSpread),
      SpreadToCurve = SpreadToCurve(HorizonSpread),
      ZeroVolSpread = ZeroVolSpread(HorizonSpread),
      YieldToMaturity = YieldToMaturity(MortgageCashFlow),
      WAL = WAL(HorizonCashFlow),
      ModDuration = ModDuration(MortgageCashFlow),
      Convexity = Convexity(MortgageCashFlow), 
      EffDuration = EffDuration(MortgageTermStructure),
      EffConvexity = EffConvexity(MortgageTermStructure),
      KeyRateTenor = unname(KeyRateTenor(MortgageTermStructure)),
      KeyRateDuration = unname(KeyRateDuration(MortgageTermStructure)),
      KeyRateConvexity = unname(KeyRateConvexity(MortgageTermStructure)),
      CouponIncome = CouponIncome,
      ScheduledPrinReceived = 
        sum(ScheduledPrin(MortgageCashFlow)[1:horizon.months]),
      PrepaidPrinReceived = 
        sum(PrepaidPrin(MortgageCashFlow)[1:horizon.months]),
      ReinvestmentIncome = ReinvestmentIncome,
      HorizonCurrBal = original.bal * MBSFactor(HorizonMBS),
      HorizonPrice = as.numeric(HorizonPrice),
      HorizonReturn = HorizonReturn,
      HorizonMos = horizon.months,
      Name = Name(Scenario),
      Type = Type(Scenario),
      ShiftType = ShiftType(Scenario),
      Shiftbps = Shiftbps(Scenario),
      Formula = ScenarioFormula(Scenario))
  }
  
  