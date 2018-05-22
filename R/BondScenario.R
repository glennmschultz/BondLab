
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
  
  #' @include ScenarioConstructor.R BondDetails.R BondCashFlows.R TermStructure.R
  #' @include BondTermStructure.R MortgageScenario.R CurveSpreads.R
  NULL
  
  
  #' @title Bond Total Return Metrics
  #' @family Bond Scenario Analysis
  #' @description 
  #' The class BondReturn holds the results of bond total return analysis.
  #' Bond total return is the sum of coupon income, principal received,
  #' reinvestment income, and price appreciation or depreciation. In addition,
  #' most investors also track horizon current balance.  The BondReturn class
  #' differs from the MortgageReturn class in that the MortgageReturn class
  #' includes scheduled principal and prepaid principal.
  #' @slot CouponIncome A numeric value the coupon income received over the 
  #' investment horizon
  #' @slot PrincipalReceived A numeric value the scheduled principal 
  #' received over the investment horizon
  #' @slot ReinvestmentIncome A numeric value the reivestment income received 
  #' over the investment horizon
  #' @slot HorizonCurrBal A numeric value the current balance at the end of the
  #' investment horizon
  #' @slot HorizonPrice A numeric the price at the end of the horizon
  #' @slot HorizonReturn A numeric value the horizon total return
  #' @slot HorizonMos A numeric value the number of months to 
  #' the scenario horizon date
  #' @exportClass BondReturn
  setClass("BondReturn",
           representation(
             CouponIncome = "numeric",
             PrincipalReceived = "numeric",
             ReinvestmentIncome = "numeric",
             HorizonCurrBal = "numeric",
             HorizonPrice = "numeric",
             HorizonReturn = "numeric",
             HorizonMos = "numeric"))
  
  # Note: standardGeneric CouponIncome is found in MortgageScenario.R
  
  #' @title PrincipalReceived generic
  #' @family Scenario Analysis
  #' @description A  generic function for method dispatch
  #' @param object an object of the type BondScenario
  setGeneric("PrincipalReceived", function(object)
    {standardGeneric("PrincipalReceived")})
  
  # Note: standardGeneric ReinvestmentIncome is found in MortgageScenario.R
  # Note: HorizonCurrBal is found in MortgageScenario.R
  # Note: HorizonPrice is found in MortgageScenario.R
  # Note: HorizonReturn is found in MortgageScenario.R
  # Note: HorizonMos is found in MortgageScenario.R

  #' @title CouponIncome method, class BondReturn
  #' @family Bond Scenario Analysis
  #' @description 
  #' A method to get the \strong{CouponIncome} paid over the scenario horizon
  #' to the investor.  Coupon income is reported as the sum of the coupon income 
  #' received by the investor.
  #' @param object An S4 class of type BondReturn
  #' @exportMethod CouponIncome
  setMethod("CouponIncome", signature("BondReturn"),
            function(object){object@CouponIncome})
  
  #' @title PrincipalReceived method, class BondReturn 
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{ScheduledPrinReceived} over the 
  #' scenario horizon to the investor.  SceheduledPrincipalReceived is reported 
  #' as the sum of scheduled principal received by the investor.
  #' @param object An S4 class of type BondReturn
  #' @exportMethod ScheduledPrinReceived
  setMethod("PrincipalReceived", signature("BondReturn"),
            function(object){object@PrincipalReceived})
  
  #' @title ReinvestmentIncome method, class BondReturn
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{ReinvestmentIncome} over the scenario
  #' horizon to the investor. ReinvestmentIncome is reported as the sum of the
  #' reinvestment income received by the investor.
  #' @param object An S4 class of type BondReturn
  #' @exportMethod ReinvestmentIncome
  setMethod("ReinvestmentIncome", signature("BondReturn"),
            function(object){object@ReinvestmentIncome})
  
  #' @title HorizonCurrBal method, class BondReturn
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{HorizonCurrBal} at the end of the
  #' scenario horizon.
  #' @param object An S4 class of type BondReturn
  #' @exportMethod HorizonCurrBal
  setMethod("HorizonCurrBal", signature("BondReturn"),
            function(object){object@HorizonCurrBal})
  
  #' @title HorizonPrice method, class BondReturn
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{HorizonPrice} at the end of the 
  #' scenario horizon.
  #' @param object An S4 class of type BondReturn
  #' @exportMethod HorizonPrice
  setMethod("HorizonPrice", signature("BondReturn"),
            function(object){object@HorizonPrice})
  
  #' @title HorizonReturn method, class BondReturn
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{HorizonReturn}
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod HorizonReturn
  setMethod("HorizonReturn", signature("BondReturn"),
            function(object){object@HorizonReturn})
  
  #' @title HorizonMos method, class BondReturn
  #' @family Bond Scenario Analysis
  #' @description A method to get \strong{HorizonMos} over which a scenario covers
  #' @param object the name of an S4 class of type BondReturn
  #' @exportMethod HorizonMos
  setMethod("HorizonMos", signature("BondReturn"),
            function(object){object@HorizonMos})
  
  #' @title BondScenario class
  #' @family Bond Scenario Analysis
  #' @description 
  #' The class \strong{BondScenario} contains the following super classes: 
  #' TermStructure, BondCashFlow, BondTermStructure, BondReturn, CurveSpreads, 
  #' and Scenario.
  #' @exportClass BondScenario
  setClass("BondScenario",
           representation(),
           contains = c("BondReturn",
                        "CurveSpreads"))
  
  setMethod("initialize",
            signature("BondScenario"),
            function(.Object,
                     ...)
              {callNextMethod(.Object,
                              ...)
              })
  
  
  #' @title BondScenario function
  #' @family Bond Scenario Analysis
  #' @description 
  #' A function to compute the \strong{total return} of a Bond.  The function 
  #' first calculates the bond cashflows as of settlment date, rolls the bond 
  #' forward per the horizon months, recomputes the cash flows, maturity and 
  #' average life and prices the bond per the user's input.  The cash flow 
  #' received, reinvestment income, and horizon price are used to compute the
  #' investor's total return.  The function returns the class BondScenario
  #' @param bond.id A character string referencing an object of the type BondDetails
  #' @param settlement.date A character string the settlement data "mm-dd-YYYY".
  #' @param rates.data A character string an object yield curve
  #' @param price A character string in decimal equivalent (.) or 32nds (-)
  #' @param principal A numeric value the par amount. 
  #' @param scenario A character string the scenario
  #' @param horizon.months A numeric value the time horizon
  #' @param method A character string the method used to fit the term structure
  #' @param ... Optional values to select term structure and horizon price method
  #' @param horizon.spot.spread A numeric value the horizon zero volatility 
  #' spread
  #' @param horizon.nominal.spread A numeric value the horizon nominal spread
  #' or spread to  the curve
  #' @param horizon.OAS A numeric value the horizon option adjusted spread
  #' (not currently implemented)
  #' @param horizon.price A numeric value the horizon price
  #' @importFrom stats loess
  #' @export BondScenario
  BondScenario <- function(bond.id = "character",
                           settlement.date = "character",
                           rates.data = "character",
                           price = "character",
                           principal = numeric(),
                           scenario = "character",
                           horizon.months = numeric(),
                           method = "dl",
                           ...,
                           horizon.spot.spread = NULL,
                           horizon.nominal.spread = NULL,
                           horizon.OAS = NULL,
                           horizon.price = NULL){
    
    # Bond Scenario analysis is done in two steps
    # The first is calculated the expected cash-flows received over the 
    # investment horizon The second is to "roll" the bond
    # forward and price the expected future cash-flows
    
    # logical declarations of scenario analysis are horzion pricing methods
    # and horizon term structure assumption used shift the sopt curve or shift
    # the coupon curve and refit the term structure model.

    if(horizon.months %% 6 != 0) stop("horizon.months not valid")
    
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
    Scenario <- ScenarioCall(Scenario = scenario)
    #set rates shift (immediate) for term structure fit
    ShiftCurve <- rates.data
    ShiftCurve[1,2:length(ShiftCurve)] <- 
      as.numeric(ScenarioFormula(Scenario)(rates.data[1,2:length(ShiftCurve)], 
                                Shiftbps = Shiftbps(Scenario)))
    
    # Set horizon curve and settlment date for horizon ending value analysis
    # This curve is used to fit the horizon term structure when the scenario is
    # not based on shift of the spot rate curve.
    
    HorizonCurve <- rates.data

    HorizonCurve[1,1] <- as.character(
      as.Date(HorizonCurve[1,1]) %m+% months(horizon.months))
    
    HorizonCurve[1,2:length(HorizonCurve)] <- 
      as.numeric(ScenarioFormula(Scenario)(rates.data[1,2:length(HorizonCurve)], 
                                Shiftbps = Shiftbps(Scenario)))
    
    HorizonSettlement <- as.Date(
      settlement.date, format = "%m-%d-%Y") %m+% months(horizon.months)
    
    Price <- PriceTypes(price = price)
    
    # fit the term structure and calcualate cashflows to compute the 
    # following Yield, WAL, Curve Spreads, KeyRate Durations, etc
    
    TermStructure <- TermStructure(
      rates.data = ShiftCurve,
      method = method)

    BondCashFlow <- BondCashFlows(bond.id = bond.id,
                                  principal = principal,
                                  settlement.date = settlement.date,
                                  price = PriceDecimalString(Price))
    
    proceeds <- Accrued(BondCashFlow) + (principal * PriceBasis(Price))
    
    
    # Compute the CurvesSpreads based on the user price and prepayment vector
    # given the user's scenario interest rate shift
    CurveSpread <- CurveSpreads(rates.data = rates.data,
                                CashFlow = BondCashFlow,
                                TermStructure = TermStructure,
                                proceeds = proceeds)


    # This section begins the  horizon bond analysis. 
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
        TimePeriod = numeric(),
        SpotRate = numeric(),
        ForwardRate = numeric(),
        DiscRate = numeric(),
        TwoYearFwd = numeric(),
        TenYearFwd = numeric())
      
      Period(HorizonTermStructure) <- Period(TermStructure)
      ForwardDate(HorizonTermStructure) <- as.character(
        as.Date(ForwardDate(TermStructure)) %m+% months(horizon.months))
      TimePeriod(HorizonTermStructure) <- TimePeriod(TermStructure)
      SpotRate(HorizonTermStructure) <- SpotRate(TermStructure)
      ForwardRate(HorizonTermStructure) <-ForwardRate(TermStructure)
      DiscRate(HorizonTermStructure) <- DiscRate(TermStructure)
      TwoYearForward(HorizonTermStructure) <- TwoYearForward(TermStructure)
      TenYearForward(HorizonTermStructure) <- TenYearForward(TermStructure)
    } else {
      HorizonTermStructure <- TermStructure(
        rates.data = HorizonCurve,
        method = method)
    } # End of if logic for term structure method


    #===========================================================================
    # This section of code rolls the bond forward in time updating
    # factor, current balance, lastpaymentdate, nextpaymentdate, wam and wala the 
    # first step is to assign bond.id to HorizonBond object
    # ==========================================================================
    
    HorizonBond <- bond.id

    paymentdates <- LastandNextPmtDate(issue.date = IssueDate(HorizonBond),
                                       dated.date = DatedDate(HorizonBond),
                                       maturity.date = Maturity(HorizonBond),
                                       settlement.date = HorizonSettlement,
                                       frequency = Frequency(HorizonBond),
                                       bond.basis = BondBasis(HorizonBond))

    HorizonBond <- `LastPmtDate<-`(HorizonBond, 
                                   as.character(paymentdates[1], format = '%m-%d-%Y'))
    HorizonBond <- `NextPmtDate<-`(HorizonBond, 
                                   as.character(paymentdates[2], format = '%m-%d-%Y'))


    # ========================================================================
    # This section begins the calculation of horizon total return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # ========================================================================
    
    PmtIndex <- which(abs(as.Date(PmtDate(BondCashFlow)) - as.Date(HorizonSettlement)) == 
                        min(abs(as.Date(PmtDate(BondCashFlow)) - as.Date(HorizonSettlement))))
    
    PmtIndex <- if(as.Date(PmtDate(BondCashFlow)[PmtIndex]) > as.Date(HorizonSettlement)) {PmtIndex -1
    } else {PmtIndex}
    
    #==========================================================================
    # From the beginning cashflow calculation get the cashflow recieved by the 
    # investor.  This should be the matrix in the book
    #==========================================================================

    CashFlowArray <- array(data = 0, dim = c(PmtIndex +2, horizon.months + 2), dimnames = NULL)
    
    # --------------------------------------------------------------------------
    # set days to 01 forces all coupon payment calculations to month year basis
    horizon.month.seq <- as.character(seq(as.Date(rates.data[1,1]), by = 'months', 
                                          length.out = horizon.months + 1),format ='%Y-01-%m')
    coupon.months <- as.character(as.Date(PmtDate(BondCashFlow)), format = "%Y-01-%m")

    colindex = NULL
    for(col in seq_along(coupon.months)){
      loc <- which(as.Date(coupon.months[col]) == as.Date(horizon.month.seq))
      if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
      } else {next()}}

    for(pmtrow in seq_along(colindex)){
      CashFlowArray[pmtrow,colindex[pmtrow]] <- CouponPmt(BondCashFlow)[pmtrow]}
    
    # To price the bond at the horizon one must first determine the scenario cash
    # flow in particular on must determine the amount of princial returned, if any,
    # over the scenario to adjust the principal outstanding of the investor holdings
    
    # -------------------------------note----------------------------------------
    # the bond cash flow engine requires an upgrade to include the slot principal
    # paid similar to that of the mortgage cash flow engine.  The engine below will
    # work for a non callable bond but will not work for sinking, putable or callable
    # bonds.  This is CashFlowBond problem and relates to the allocation of principal
    prin.months <- as.character(as.Date(PmtDate(BondCashFlow)), format = "%Y-01-%m")
    
    colindex = NULL
    for(col in seq_along(prin.months)){
      loc <- which(as.Date(prin.months[col]) == as.Date(horizon.month.seq))
      if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
      } else {next()}}
    
    for(pmtrow in seq_along(colindex)){
      CashFlowArray[PmtIndex + 1,colindex[pmtrow]] <- principal - PrincipalOutstanding(BondCashFlow)[pmtrow]}
    
    CashFlowArray[PmtIndex + 1] <- sum(CashFlowArray[PmtIndex + 1,])
    horizon.principal <- principal - CashFlowArray[PmtIndex + 1,13]

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
    "spot" = ZVSpreadToPriceBond(bond.id = HorizonBond,
                                 settlement.date = HorizonSettlement,
                                 term.structure = HorizonTermStructure,
                                 ZV.spread = horizon.spot.spread),
   "nominal" = SpreadToPriceBond(bond.id = HorizonBond,
                                 rates.data = HorizonCurve,
                                 settlement.date = HorizonSettlement,
                                 spread = horizon.nominal.spread),
   "price" = PriceTypes(horizon.price))

    HorizonCashFlow <- BondCashFlows(bond.id = HorizonBond,
                                     principal = principal,
                                     settlement.date = HorizonSettlement,
                                     price = PriceDecimalString(HorizonPrice))

    HorizonProceeds <- (PriceBasis(HorizonPrice) * principal) + Accrued(HorizonCashFlow)

    HorizonSpread <- CurveSpreads(
      rates.data = HorizonCurve,
      CashFlow = HorizonCashFlow,
      TermStructure = HorizonTermStructure,
      proceeds = HorizonProceeds)
    
    #---------------------------------------------------------------------------
    #Allocate reinvestment income to the bond cash flows
    #---------------------------------------------------------------------------

    reinvestment.rate <- as.numeric(HorizonCurve[1,2])/yield.basis
    
    for(rr in 1:nrow(CashFlowArray-1)){
      for(month in 1:horizon.months + 1){
        if(month == 1){CashFlowArray[rr,month] = CashFlowArray[rr,month]
        } else {CashFlowArray[rr,month] = CashFlowArray[rr,month] + 
          CashFlowArray[rr, month-1] *(1 + reinvestment.rate/months.in.year)}
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

    CouponIncome <- sum(CouponPmt(BondCashFlow)[1:PmtIndex])
    ReceivedCashFlow <- TotalCashFlow(BondCashFlow)[1:PmtIndex]

    TerminalValue <-  sum(CashFlowArray[,ncol(CashFlowArray)])
    ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow))

    # This needs to be changed by adding principal pmt to bond cashflow class
    # and bond cashflow engine. 
    PrincipalRepaid <- sum(TotalCashFlow(BondCashFlow)[1:PmtIndex]) - sum(CouponPmt(BondCashFlow)[1:PmtIndex])

    HorizonReturn <- (TerminalValue/proceeds)^(1/(months.in.year/horizon.months))
    HorizonReturn <- (HorizonReturn - 1) * yield.basis

    new("BondScenario",
        BenchMark = BenchMark(HorizonSpread),
        SpreadToBenchmark = SpreadToBenchmark(HorizonSpread),
        SpreadToCurve = SpreadToCurve(HorizonSpread),
        ZeroVolSpread = ZeroVolSpread(HorizonSpread),
        CouponIncome = CouponIncome,
        PrincipalReceived = PrincipalRepaid,
        ReinvestmentIncome = ReinvestmentIncome,
        HorizonCurrBal = principal - PrincipalRepaid,
        HorizonPrice = PriceDecimal(HorizonPrice),
        HorizonReturn = HorizonReturn,
        HorizonMos = horizon.months)
  }