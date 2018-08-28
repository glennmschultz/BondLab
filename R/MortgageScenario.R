  
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
           contains = c("MortgageReturn",
                        "ModelToCPR",
                        "CurveSpreads"))

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
  #' @param price A character string in decimal equivalent (.) or 32nds (-)
  #' @param original.bal A numeric value the price
  #' @param scenario.curves A character string an object of type ScenarioCurves
  #' @param prepayment A character string the prepayment assumption
  #' @param ... Optional values when PSA or CPR is used or Yield Curve
  #' is used
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
  #' @importFrom stats loess
  #' @export
  MortgageScenario  <- function(bond.id = "character",
                          settlement.date = "character",
                          price = "character", 
                          original.bal = numeric(),
                          scenario.curves = "character",
                          prepayment,
                          ...,
                          horizon.spot.spread = NULL,
                          horizon.nominal.spread = NULL,
                          horizon.OAS = NULL,
                          horizon.price = NULL,
                          begin.cpr,
                          end.cpr,
                          seasoning.period,
                          CPR) { 
    
   
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
    Price <- PriceTypes(price)
    startcurve <- as.data.frame(StartCurve(scenario.curves), stringsAsFactors = FALSE)
    starttermstrc <- StartTermStrc(scenario.curves)
    horizoncurve <- as.data.frame(HorizonCurve(scenario.curves), stringsAsFactors = FALSE)
    horizontermstrc <- HorizonTermStrc(scenario.curves)
    horizonmonths = ScenarioHorizonMos(scenario.curves)
    MortgageRate <- ProjectMortgageRate(bond.id = bond.id, term.structure = term.structure)
    ModelTune <- ModelTune(bond.id = bond.id)

   
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

    # This section of code rolls the MBS pass-though forward in time updating
    # factor, current balance, lastpaymentdate, nextpaymentdate, wam and wala the 
    # first step is to assign bond.id to HorizonMBS object
    HorizonMBS <- bond.id
    
    paymentdates <- LastandNextPmtDate(issue.date = IssueDate(HorizonMBS),
                                       dated.date = DatedDate(HorizonMBS),
                                       maturity.date = Maturity(HorizonBond),
                                       settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                                      format = '%m-%d-%Y'),
                                       frequency = Frequency(HorizonMBS),
                                       bond.basis = BondBasis(HorizonMBS))
    
    # Use the projected cashflow to determine the current balance outstanding 
    # at the end of horizon
    SchedPrincipal <- ScheduledPrin(MortgageCashFlow)[1:horizon.months]
    PrepaidPrincipal <- PrepaidPrin(MortgageCashFlow)[1:horizon.months]
    DefaultedPrincipal <- DefaultedPrin(MortgageCashFlow)[1:horizon.months]
    TotalPrincipal <- sum(SchedPrincipal) + 
    sum(PrepaidPrincipal) + sum(DefaultedPrincipal)
    
    # Update the LastPmtDate and NextPmtDate to reflect the end of the horizon
    # and compute and update the MBSFactor, CurrentBal, WAM, and WALA
    HorizonMBS <- `LastPmtDate<-`(HorizonMBS, 
                                  as.character(paymentdates[1], format = '%m-%d-%Y'))
    
    HorizonMBS <- `NextPmtDate<-`(HorizonMBS, 
                                  as.character(paymentdates[2], format = '%m-%d-%Y'))
      
    MBSFactor(HorizonMBS) <- ((original.bal * MBSFactor(bond.id)) - TotalPrincipal)/ original.bal
    
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
    
    # ========================================================================
    # This section begins the calculation of horizon total return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # ========================================================================
    
    PmtIndex <- which(abs(as.Date(PmtDate(BondCashFlow)) - as.Date(horizoncurve[1,1])) ==
                        min(abs(as.Date(PmtDate(BondCashFlow)) - as.Date(horizoncurve[1,1]))))[1]
    
    PmtIndex <- if(as.Date(PmtDate(BondCashFlow)[PmtIndex]) > as.Date(horizoncurve[1,1])) {PmtIndex -1
    } else {PmtIndex}
    
    #==========================================================================
    # From the beginning cashflow calculation get the cashflow recieved by the 
    # investor.  This should be the matrix in the book
    #==========================================================================
    
    CashFlowArray <- array(data = 0, dim = c(PmtIndex +2, horizonmonths + 2), dimnames = NULL)
    
    # --------------------------------------------------------------------------
    # set days to 01 forces all coupon payment calculations to month year basis
    startdate <- paste(substr(startcurve[1,1],1,8),'01', sep ="")
    horizonmonths.seq <- as.character(seq(as.Date(startdate), by = 'months', 
                                          length.out = horizonmonths + 1),format ='%Y-%m-%d')
    coupon.months <- as.character(as.Date(PmtDate(MortgageCashFlow)), format = "%Y-%m-01")
    
    colindex = NULL
    for(col in seq_along(coupon.months)){
      loc <- which(as.Date(coupon.months[col]) == as.Date(horizonmonths.seq))
      if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
      } else {next()}}
    
    for(pmtrow in seq_along(colindex)){
      CashFlowArray[pmtrow,colindex[pmtrow]] <- CouponPmt(MortgageCashFlow)[pmtrow]}
    
    # To price the bond at the horizon one must first determine the scenario cash
    # flow in particular on must determine the amount of princial returned, if any,
    # over the scenario to adjust the principal outstanding of the investor holdings
    
    # -------------------------------note----------------------------------------
    # the bond cash flow engine requires an upgrade to include the slot principal
    # paid similar to that of the mortgage cash flow engine.  The engine below will
    # work for a non callable bond but will not work for sinking, putable or callable
    # bonds.  This is CashFlowBond problem and relates to the allocation of principal
    prin.months <- as.character(as.Date(PmtDate(MortgageCashFlow)), format = "%Y-%m-01")
    
    colindex = NULL
    for(col in seq_along(prin.months)){
      loc <- which(as.Date(prin.months[col]) == as.Date(horizonmonths.seq))
      if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
      } else {next()}}
    
    for(pmtrow in seq_along(colindex)){
      CashFlowArray[PmtIndex + 1,colindex[pmtrow]] <- principal - PrincipalOutstanding(MortgageCashFlow)[pmtrow]}
    
    CashFlowArray[PmtIndex + 1] <- sum(CashFlowArray[PmtIndex + 1,])
    horizon.principal <- principal - CashFlowArray[PmtIndex + 1,horizonmonths-1]
    
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
    # calculate discount rates
    InterpolateSpot <- splines::interpSpline(
      TimePeriod(HorizonTermStructure)[1:360],
      #difftime(as.Date(ForwardDate(HorizonTermStructure)[1:360]),
      #         TradeDate(HorizonTermStructure))/30,
      SpotRate(HorizonTermStructure)[1:360],
      bSpline = TRUE)
    
    SpotRates <- predict(
      InterpolateSpot, TimePeriod(HorizonCashFlow))
      #difftime(as.Date(PmtDate(HorizonCashFlow)),
      #         as.Date(TradeDate(HorizonTermStructure)))/30)
    
    n.period = as.numeric(difftime(as.Date(
      PmtDate(HorizonCashFlow)),
      as.Date(TradeDate(HorizonTermStructure)))/30) / months.in.year
    
    DiscountRate <- 
      (1+((SpotRates$y + horizon.spot.spread)/yield.basis))^ n.period
    DiscountRate <- 1/DiscountRate
    
    HorizonPresentValue <- 
      DiscountRate * TotalCashFlow(HorizonCashFlow)
    PresentValue <- sum(HorizonPresentValue)
    
    return(PresentValue)
    }
  
  # Do not replace this with curve spreads as this section of code is used 
  # to compute horizon yield to maturity from nominal spread and interpolated
  # curve.  
  Horizon.Nominal.Value <- function(HorizonCurve = "character",
                           HorizonTermStructure = "character",
                          HorizonCashFlow = "character"){
  
  InterpolateCurve <- splines::interpSpline(as.numeric(rates.data[2,2:12]),
                                            as.numeric(rates.data[1,2:12]),
                                            bSpline = TRUE)
  
  #InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
  #                          as.numeric(rates.data[2,2:12]),
  #                          data = data.frame(HorizonCurve))
      
  HorizonYield <- 
      predict(InterpolateCurve, WAL(HorizonCashFlow))$y + horizon.nominal.spread
      HorizonYield <- rep(HorizonYield, NumberofCashFlow)
      DiscountRate <- 1/((1 + (HorizonYield/monthly.yield.basis))^
                           (Period(HorizonTermStructure)[1:NumberofCashFlow]))
      HorizonPresentValue <- DiscountRate * TotalCashFlow(HorizonCashFlow)
      PresentValue <- sum(HorizonPresentValue)
      return(PresentValue)}
  
  # Horizon.Price.Value is a function which returns the principal proceeds
  # at the horizon date and is used to calculate PresentValue
  Horizon.Price.Value <- function(HorizonBond,
                                    HorizonPrice){
      HorizonPrice <- PriceTypes(HorizonPrice)
      original.bal * MBSFactor(HorizonBond) * (PriceBasis(HorizonPrice))
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
  
  HorizonPrice <- PriceTypes(price = HorizonPrice)
  
  HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                    original.bal = original.bal,
                    settlement.date = HorizonSettlement,
                    price = PriceDecimalString(HorizonPrice),
                    PrepaymentAssumption = HorizonPrepaymentAssumption)
  
  HorizonProceeds <- ((PriceBasis(HorizonPrice) * 
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
      CPRLife = CPRLife(LifeCPR),
      BenchMark = BenchMark(HorizonSpread),
      SpreadToBenchmark = SpreadToBenchmark(HorizonSpread),
      SpreadToCurve = SpreadToCurve(HorizonSpread),
      ZeroVolSpread = ZeroVolSpread(HorizonSpread),
      CouponIncome = CouponIncome,
      ScheduledPrinReceived = 
        sum(ScheduledPrin(MortgageCashFlow)[1:horizon.months]),
      PrepaidPrinReceived = 
        sum(PrepaidPrin(MortgageCashFlow)[1:horizon.months]),
      ReinvestmentIncome = ReinvestmentIncome,
      HorizonCurrBal = original.bal * MBSFactor(HorizonMBS),
      HorizonPrice = PriceDecimal(HorizonPrice),
      HorizonReturn = HorizonReturn,
      HorizonMos = horizon.months)
  }
  