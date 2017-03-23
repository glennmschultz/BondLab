
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
  
  #' @include ScenarioConstructor.R BondCashFlows.R TermStructure.R
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
           contains = c("TermStructure",
                        "BondCashFlows",
                        "BondTermStructure",
                        "BondReturn",
                        "CurveSpreads",
                        "Scenario"))
  
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
    
    BondTermStructure = BondTermStructure(bond.id = bond.id,
                                          Rate.Delta = rate.delta,
                                          TermStructure = TermStructure,
                                          principal = principal,
                                          price = PriceDecimalString(Price),
                                          cashflow = BondCashFlow)

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
        method = method)
    } # End of if logic for term structure method


    # This section of code rolls the bond forward in time updating
    # factor, current balance, lastpaymentdate, nextpaymentdate, wam and wala the 
    # first step is to assign bond.id to HorizonMBS object
    
    HorizonBond <- bond.id
    HorizonBond <- `LastPmtDate<-`(HorizonBond,
                                   as.character(
                                     format(as.Date(
                                       LastPmtDate(bond.id), 
                        format = "%m-%d-%Y") %m+% months(horizon.months), 
                        "%m-%d-%Y")))
    
    HorizonBond <- `NextPmtDate<-`(HorizonBond,
                                   as.character(format(
                                     as.Date(NextPmtDate(bond.id), 
                      format = "%m-%d-%Y") %m+% months(horizon.months), 
                      "%m-%d-%Y")))
    
    HorizonCashFlow <- BondCashFlows(bond.id = HorizonBond,
                                     principal = principal,
                                     settlement.date = HorizonSettlement,
                                     price = PriceDecimalString(Price))

    # ========================================================================
    # This section begins the calculation of horizon total return
    # Cashflow Received + Reinvestment Income + Present Value at Horizon
    # ========================================================================
    
    NumberofCashFlow <- as.numeric(length(TotalCashFlow(HorizonCashFlow)))
    reinvestment.rate <- as.numeric(HorizonCurve[1,2])/yield.basis

    # Here frequency and horizon is converted into the number of payments 
    # received Frequency is the number of payments recieved in a year and the 
    # monthly interval between payments.  Maybe frequency should be months 
    # between payments
    NumberPaymentReceived <- horizon.months/(horizon.months/Frequency(bond.id))

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
    
    Horizon.Spot.Value <- function(HorizonTermStructure,
                                   HorizonCashFlow,
                                   HorizonSpotSpread,
                                   NumberofCashFlow){
      # calculate discount rates
      InterpolateSpot <- splines::interpSpline(
        difftime(as.Date(ForwardDate(HorizonTermStructure)[1:360]),
                 TradeDate(HorizonTermStructure))/30,
        SpotRate(HorizonTermStructure)[1:360],
        bSpline = TRUE)
      
      SpotRates <- predict(
        InterpolateSpot,
        difftime(as.Date(PmtDate(HorizonCashFlow)),
                 as.Date(TradeDate(HorizonTermStructure)))/30)
      
      n.period = as.numeric(difftime(as.Date(PmtDate(HorizonCashFlow)),
                   as.Date(TradeDate(HorizonTermStructure)))/30) / months.in.year
      
      DiscountRate <- 
        (1+((SpotRates$y + horizon.spot.spread)/yield.basis))^ n.period
      DiscountRate <- 1/DiscountRate
      
      HorizonPresentValue <- 
      DiscountRate * TotalCashFlow(HorizonCashFlow)
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
      if(LastPmtDate(HorizonBond) == Maturity(HorizonBond)){0
    }else {principal * (HorizonPrice/price.basis)}
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
    
    # Here will need to subtract from sinking fund scheduled principal paid
    HorizonPrice <- if(horizon.price.type == "price"){horizon.price} else {
      (PresentValue / principal) * price.basis}
    
    # Replace this with PriceTypes objects  
    HorizonPrice <- sprintf("%.8f", HorizonPrice)

    # Replace this with an array of cashflow

    HorizonCashFlow <- BondCashFlows(bond.id = HorizonBond,
                                     principal = principal,
                                     settlement.date = HorizonSettlement,
                                     price = PriceDecimalString(Price))

    HorizonProceeds <- (((as.numeric(HorizonPrice)/price.basis) * principal) + 
                          Accrued(HorizonCashFlow))

    HorizonSpread <- CurveSpreads(
      rates.data = HorizonCurve,
      CashFlow = HorizonCashFlow,
      TermStructure = HorizonTermStructure,
      proceeds = HorizonProceeds)
    
    # From the beginning cashflow calculation get the cashflow recieved by the 
    # investor.  This should be the matrix in the book
      
    CouponIncome <- sum(CouponPmt(BondCashFlow)[1:NumberPaymentReceived])
    ReceivedCashFlow <- TotalCashFlow(BondCashFlow)[1:NumberPaymentReceived]

    n.period <- 
      as.numeric(difftime(as.Date(PmtDate(BondCashFlow)[NumberPaymentReceived]), 
                          as.Date(PmtDate(BondCashFlow)[1:NumberPaymentReceived]), 
                          units = "days")/days.in.month)

    TerminalValue <- 
    ReceivedCashFlow * ((1 + (reinvestment.rate/months.in.year)) ^ (n.period))
    ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow))
    
    # This needs to be changed by adding principal pmt to bond cashflow class
    # and bond cashflow engine. 
    PrincipalRepaid <- sum(TotalCashFlow(BondCashFlow)[1:NumberPaymentReceived]) - 
    sum(CouponPmt(BondCashFlow)[1:NumberPaymentReceived])

    HorizonValue <- 
      CouponIncome + 
      ReinvestmentIncome + 
      PrincipalRepaid + 
      PresentValue
    
    HorizonReturn <- (HorizonValue/proceeds)^(months.in.year/horizon.months)
    HorizonReturn <- (HorizonReturn - 1) * yield.basis
    
    new("BondScenario",
        Period = Period(BondCashFlow),
        PmtDate = PmtDate(BondCashFlow),
        TimePeriod = TimePeriod(BondCashFlow),
        PrincipalOutstanding = PrincipalOutstanding(BondCashFlow),
        CouponPmt = CouponPmt(BondCashFlow),
        TotalCashFlow = TotalCashFlow(BondCashFlow),
        SpotRate = SpotRate(TermStructure),
        ForwardRate = ForwardRate(TermStructure),
        TwoYearFwd = TwoYearForward(TermStructure),
        TenYearFwd = TenYearForward(TermStructure),
        BenchMark = BenchMark(HorizonSpread),
        SpreadToBenchmark = SpreadToBenchmark(HorizonSpread),
        SpreadToCurve = SpreadToCurve(HorizonSpread),
        ZeroVolSpread = ZeroVolSpread(HorizonSpread),
        YieldToMaturity = YieldToMaturity(BondCashFlow),
        WAL = WAL(BondCashFlow),
        ModDuration = ModDuration(BondCashFlow),
        Convexity = Convexity(BondCashFlow), 
        EffDuration = EffDuration(BondTermStructure),
        EffConvexity = EffConvexity(BondTermStructure),
        KeyRateTenor = unname(KeyRateTenor(BondTermStructure)),
        KeyRateDuration = unname(KeyRateDuration(BondTermStructure)),
        KeyRateConvexity = unname(KeyRateConvexity(BondTermStructure)),
        CouponIncome = CouponIncome,
        PrincipalReceived = PrincipalRepaid,
        ReinvestmentIncome = ReinvestmentIncome,
        HorizonCurrBal = principal - PrincipalRepaid,
        HorizonPrice = as.numeric(HorizonPrice),
        HorizonReturn = HorizonReturn,
        HorizonMos = horizon.months,
        Name = Name(Scenario),
        Type = Type(Scenario),
        ShiftType = ShiftType(Scenario),
        Shiftbps = Shiftbps(Scenario),
        Formula = ScenarioFormula(Scenario))
  }