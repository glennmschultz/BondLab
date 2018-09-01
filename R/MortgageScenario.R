  
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
                                          begin.cpr = NULL,
                                          end.cpr = NULL,
                                          seasoning.period = NULL,
                                          cpr = NULL,
                                          cdr = NULL)
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
  #' @param prepayment.assumption A character string the prepayment assumption
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
  #' @param cpr A numeric value the CPR speed
  #' @param severity A numeric value the mortgage loss severity assumption
  #' @importFrom stats loess
  #' @export
  MortgageScenario  <- function(bond.id,
                          settlement.date,
                          price, 
                          original.bal,
                          scenario.curves,
                          prepayment.assumption,
                          ...,
                          horizon.spot.spread = NULL,
                          horizon.nominal.spread = NULL,
                          horizon.OAS = NULL,
                          horizon.price = NULL,
                          begin.cpr = NULL,
                          end.cpr = NULL,
                          seasoning.period = NULL,
                          cpr = NULL,
                          severity = NULL) { 
    
   
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
    mbs.factor <- MBSFactor(bond.id)
    principal <- original.bal * mbs.factor
    Price <- PriceTypes(price)
    
    startcurve <- as.data.frame(StartCurve(scenario.curves), stringsAsFactors = FALSE)
    starttermstrc <- StartTermStrc(scenario.curves)
    horizoncurve <- as.data.frame(HorizonCurve(scenario.curves), stringsAsFactors = FALSE)
    horizontermstrc <- HorizonTermStrc(scenario.curves)
    horizonmonths = ScenarioHorizonMos(scenario.curves)
    
    Prepayment <- PrepaymentModel(
      bond.id = bond.id,
      term.structure = starttermstrc,
      PrepaymentAssumption = prepayment.assumption,
      begin.cpr = begin.cpr,
      end.cpr = end.cpr,
      seasoning.period = seasoning.period,
      cpr = cpr,
      severity = 0)
    
    MortgageCashFlow <- MortgageCashFlow(
      bond.id = bond.id,
      original.bal = original.bal,
      settlement.date = settlement.date,
      price = PriceDecimalString(Price),
      PrepaymentAssumption = Prepayment)

    proceeds <- Accrued(MortgageCashFlow) + (principal * PriceBasis(Price))

    # Compute the CurvesSpreads based on the user price and prepayment vector
    # given the user's scenario interest rate shift
    CurveSpread <- CurveSpreads(rates.data = startcurve,
                                CashFlow = MortgageCashFlow,
                                TermStructure = starttermstrc,
                                proceeds = proceeds)
    
    # Compute life CPR using the base case term structure fit
    # stop here refactor ModelToCPR function
    
    LifeCPR <- ModelToCPR(bond.id = bond.id,
                          settlement.date = settlement.date,
                          term.structure = starttermstrc,
                          original.bal = original.bal,
                          price = PriceDecimalString(Price),
                          yield = YieldToMaturity(MortgageCashFlow))

    # This section of code rolls the MBS pass-though forward in time updating
    # factor, current balance, lastpaymentdate, nextpaymentdate, wam and wala the 
    # first step is to assign bond.id to HorizonMBS object
    HorizonMBS <- bond.id
    
    paymentdates <- LastandNextPmtDate(issue.date = IssueDate(HorizonMBS),
                                       dated.date = DatedDate(HorizonMBS),
                                       maturity.date = Maturity(HorizonMBS),
                                       settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                                      format = '%m-%d-%Y'),
                                       frequency = Frequency(HorizonMBS),
                                       bond.basis = BondBasis(HorizonMBS))
    
    # Use the projected cashflow to determine the current balance outstanding 
    # at the end of horizon
    SchedPrincipal <- sum(ScheduledPrin(MortgageCashFlow)[1:horizonmonths])
    PrepaidPrincipal <- sum(PrepaidPrin(MortgageCashFlow)[1:horizonmonths])
    DefaultedPrincipal <- sum(DefaultedPrin(MortgageCashFlow)[1:horizonmonths])
    TotalPrincipal <- SchedPrincipal + PrepaidPrincipal + DefaultedPrincipal
    
    # Update the LastPmtDate and NextPmtDate to reflect the end of the horizon
    # and compute and update the MBSFactor, CurrentBal, WAM, and WALA
    HorizonMBS <- `LastPmtDate<-`(HorizonMBS, 
                                  as.character(paymentdates[1], format = '%m-%d-%Y'))
    
    HorizonMBS <- `NextPmtDate<-`(HorizonMBS, 
                                  as.character(paymentdates[2], format = '%m-%d-%Y'))
      
    MBSFactor(HorizonMBS) <- ((original.bal * mbs.factor) - TotalPrincipal)/ original.bal
    
    CurrentBal(HorizonMBS) <- CurrentBal(bond.id) - TotalPrincipal
    WAM(HorizonMBS) <- WAM(bond.id) - horizonmonths
    WALA(HorizonMBS) <- WALA(bond.id) + horizonmonths
    
    HorizonPrepayment <- PrepaymentModel(
      bond.id = bond.id,
      term.structure = horizontermstrc,
      PrepaymentAssumption = prepayment.assumption,
      begin.cpr = begin.cpr,
      end.cpr = end.cpr,
      seasoning.period = seasoning.period,
      cpr = cpr,
      severity = 0)

    HorizonCashFlow <- MortgageCashFlow(
      bond.id = HorizonMBS,
      original.bal = original.bal,
      settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                     format = '%m-%d-%Y'),
      price = PriceDecimalString(Price),
      PrepaymentAssumption = HorizonPrepayment)
    
    # ========================================================================
    # This section begins the calculation of horizon total return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # ========================================================================
    
    PmtIndex <- which(
      abs(as.Date(PmtDate(MortgageCashFlow)) - as.Date(horizoncurve[1,1])) ==
      min(abs(as.Date(PmtDate(MortgageCashFlow)) - as.Date(horizoncurve[1,1])))
      )[1]
    
    PmtIndex <- 
      if(as.Date(PmtDate(MortgageCashFlow)[PmtIndex]) > as.Date(horizoncurve[1,1])) 
      {PmtIndex -1} else {PmtIndex}
    
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
      CashFlowArray[pmtrow,colindex[pmtrow]] <- PassThroughInterest(MortgageCashFlow)[pmtrow]}
    
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
      CashFlowArray[PmtIndex + 1,colindex[pmtrow]] <- 
        TotalCashFlow(MortgageCashFlow)[pmtrow] - PassThroughInterest(MortgageCashFlow)[pmtrow]}
    
    CashFlowArray[PmtIndex + 1] <- sum(CashFlowArray[PmtIndex + 1,])
    horizon.principal <- original.bal * MBSFactor(HorizonMBS)
    
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

    HorizonPrice <- switch(horizon.price.type,
                           "spot" = ZVSpreadToPriceMBS(
                             bond.id = HorizonMBS,
                             settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                            format = '%m-%d-%Y'),
                             term.structure = horizontermstrc,
                             prepayment.assumption = HorizonPrepayment,
                             ZV.spread = horizon.spot.spread),
                           
                           "nominal" = SpreadToPriceMBS(
                             bond.id = HorizonMBS,
                             rates.data = horizoncurve,
                             settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                            format = '%m-%d-%Y'),
                             prepayment.assumption = HorizonPrepayment,
                             spread = horizon.nominal.spread),
                           
                           "price" = PriceTypes(horizon.price))
  
  
  HorizonCashFlow <- MortgageCashFlow(bond.id = HorizonMBS,
                    original.bal = original.bal,
                    settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                   format = '%m-%d-%Y'),
                    price = PriceDecimalString(HorizonPrice),
                    PrepaymentAssumption = HorizonPrepayment)
  
  HorizonProceeds <- (
    (PriceBasis(HorizonPrice) * MBSFactor(HorizonMBS) *original.bal) + 
      Accrued(HorizonCashFlow)
    )

  HorizonSpread <- CurveSpreads(
    rates.data = horizoncurve,
    CashFlow = HorizonCashFlow,
    TermStructure = horizontermstrc,
    proceeds = HorizonProceeds)
  
  #---------------------------------------------------------------------------
  #Allocate reinvestment income to the bond cash flows
  #---------------------------------------------------------------------------
  reinvestment.rate <- as.numeric(horizoncurve[1,2])/(yield.basis * months.in.year)
  
  for(rr in 1:nrow(CashFlowArray-1)){
    for(month in 1:horizonmonths + 1){
      if(month == 1){CashFlowArray[rr,month] = CashFlowArray[rr,month]
      } else {CashFlowArray[rr,month] = CashFlowArray[rr,month] + 
        CashFlowArray[rr, month-1] *(1 + reinvestment.rate)}
    }
  }
  
  #---------------------------------------------------------------------------
  #Aggregate Cashflows 
  #---------------------------------------------------------------------------
  
  for(row in 1:nrow(CashFlowArray-1)){
    CashFlowArray[row, ncol(CashFlowArray)] = CashFlowArray[row,ncol(CashFlowArray)-1]
  }
  
  
  #---------------------------------------------------------------------------
  #Assign horizon proceeds to array
  #---------------------------------------------------------------------------
  
  CashFlowArray[nrow(CashFlowArray), ncol(CashFlowArray)] = HorizonProceeds
  
  CouponIncome <- sum(PassThroughInterest(MortgageCashFlow)[1:PmtIndex])
  ReceivedCashFlow <- TotalCashFlow(MortgageCashFlow)[1:PmtIndex]
  
  TerminalValue <-  sum(CashFlowArray[,ncol(CashFlowArray)])
  ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow)- HorizonProceeds)
  
  # This needs to be changed by adding principal pmt to bond cashflow class
  # and bond cashflow engine. 
  PrincipalRepaid <- sum(TotalCashFlow(MortgageCashFlow)[1:PmtIndex]) - sum(PassThroughInterest(MortgageCashFlow)[1:PmtIndex])
  
  HorizonReturn <- (TerminalValue/proceeds)^(1/(months.in.year/horizonmonths))
  HorizonReturn <- (HorizonReturn - 1) * yield.basis
  
    
  new("MortgageScenario",
      CPRLife = CPRLife(LifeCPR),
      BenchMark = BenchMark(HorizonSpread),
      SpreadToBenchmark = SpreadToBenchmark(HorizonSpread),
      SpreadToCurve = SpreadToCurve(HorizonSpread),
      ZeroVolSpread = ZeroVolSpread(HorizonSpread),
      CouponIncome = CouponIncome,
      ScheduledPrinReceived = 
        sum(ScheduledPrin(MortgageCashFlow)[1:horizonmonths]),
      PrepaidPrinReceived = 
        sum(PrepaidPrin(MortgageCashFlow)[1:horizonmonths]),
      ReinvestmentIncome = ReinvestmentIncome,
      HorizonCurrBal = original.bal * MBSFactor(HorizonMBS),
      HorizonPrice = PriceDecimal(HorizonPrice),
      HorizonReturn = HorizonReturn,
      HorizonMos = horizonmonths)
  }
  