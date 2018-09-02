
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
  #' @description A method to get \strong{Principal Received} over the 
  #' scenario horizon to the investor.  Principal Received is reported 
  #' as the sum of scheduled principal received by the investor.
  #' @param object An S4 class of type BondReturn
  #' @exportMethod PrincipalReceived
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
  #' @param scenario.curves A character string an object of type ScenarioCurves
  #' @param price A character string in decimal equivalent (.) or 32nds (-)
  #' @param principal A numeric value the par amount. 
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
                           scenario.curves = "character",
                           price = "character",
                           principal = numeric(),
                           ...,
                           horizon.spot.spread = NULL,
                           horizon.nominal.spread = NULL,
                           horizon.OAS = NULL,
                           horizon.price = NULL){
    

    bond.id <- bond.id
    Price <- PriceTypes(price)
    startcurve <- as.data.frame(StartCurve(scenario.curves), stringsAsFactors = FALSE)
    starttermstrc <- StartTermStrc(scenario.curves)
    horizoncurve <- as.data.frame(HorizonCurve(scenario.curves), stringsAsFactors = FALSE)
    horizontermstrc <- HorizonTermStrc(scenario.curves)
    horizonmonths = ScenarioHorizonMos(scenario.curves)

    #if(horizonmonths %% 6 != 0) stop("horizon.months not valid")
    
    if(is.null(horizon.spot.spread) != TRUE) {
      horizon.price.type <- "spot"
    } else if(is.null(horizon.nominal.spread) != TRUE) {
      horizon.price.type <- "nominal"  
    } else if(is.null(horizon.OAS) != TRUE) {
      horizon.price.type <- "oas"  
    } else {
      horizon.price.type <- "price"
    }

    BondCashFlow <- BondCashFlows(bond.id = bond.id,
                                  principal = principal,
                                  settlement.date = settlement.date,
                                  price = PriceDecimalString(Price))
    
    proceeds <- Accrued(BondCashFlow) + (principal * PriceBasis(Price))
    
    # Compute the CurvesSpreads based on the user price and prepayment vector
    # given the user's scenario interest rate shift
    CurveSpread <- CurveSpreads(rates.data = startcurve,
                                CashFlow = BondCashFlow,
                                TermStructure = starttermstrc,
                                proceeds = proceeds)


    #===========================================================================
    # This section of code rolls the bond forward in time updating
    # factor, current balance, lastpaymentdate, nextpaymentdate, wam and wala the 
    # first step is to assign bond.id to HorizonBond object
    # ==========================================================================
    
    HorizonBond <- bond.id

    paymentdates <- LastandNextPmtDate(issue.date = IssueDate(HorizonBond),
                                       dated.date = DatedDate(HorizonBond),
                                       maturity.date = Maturity(HorizonBond),
                                       settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                                      format = '%m-%d-%Y'),
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
    coupon.months <- as.character(as.Date(PmtDate(BondCashFlow)), format = "%Y-%m-01")

    colindex = NULL
    for(col in seq_along(coupon.months)){
      loc <- which(as.Date(coupon.months[col]) == as.Date(horizonmonths.seq))
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
    prin.months <- as.character(as.Date(PmtDate(BondCashFlow)), format = "%Y-%m-01")
    
    colindex = NULL
    for(col in seq_along(prin.months)){
      loc <- which(as.Date(prin.months[col]) == as.Date(horizonmonths.seq))
      if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
      } else {next()}}
    
    for(pmtrow in seq_along(colindex)){
      CashFlowArray[PmtIndex + 1,colindex[pmtrow]] <- principal - PrincipalOutstanding(BondCashFlow)[pmtrow]}
    
    #possible bug here maybe CashFlowArray[PmtIndex + 1,ncol]
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
    
    HorizonPrice <- switch(horizon.price.type,
                           "spot" = ZVSpreadToPriceBond(bond.id = HorizonBond,
                                                        settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                                                       format = '%m-%d-%Y'),
                                                        term.structure = horizontermstrc,
                                                        ZV.spread = horizon.spot.spread),
                           "nominal" = SpreadToPriceBond(bond.id = HorizonBond,
                                                         rates.data = horizoncurve,
                                                         settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                                                        format = '%m-%d-%Y'),
                                                         spread = horizon.nominal.spread),
                           "price" = PriceTypes(horizon.price))

    HorizonCashFlow <- BondCashFlows(bond.id = HorizonBond,
                                     principal = principal,
                                     settlement.date = as.character(as.Date(horizoncurve[1,1]), 
                                                                    format = '%m-%d-%Y'),
                                     price = PriceDecimalString(HorizonPrice))

    HorizonProceeds <- (PriceBasis(HorizonPrice) * principal) + Accrued(HorizonCashFlow)

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

    CouponIncome <- sum(CouponPmt(BondCashFlow)[1:PmtIndex])
    ReceivedCashFlow <- TotalCashFlow(BondCashFlow)[1:PmtIndex]

    TerminalValue <-  sum(CashFlowArray[,ncol(CashFlowArray)])
    ReinvestmentIncome <- as.numeric(sum(TerminalValue) - sum(ReceivedCashFlow)- HorizonProceeds)

    # This needs to be changed by adding principal pmt to bond cashflow class
    # and bond cashflow engine. 
    PrincipalRepaid <- sum(TotalCashFlow(BondCashFlow)[1:PmtIndex]) - sum(CouponPmt(BondCashFlow)[1:PmtIndex])

    HorizonReturn <- (TerminalValue/proceeds)^(1/(months.in.year/horizonmonths))
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
        HorizonMos = horizonmonths)
  }