
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.
  
  # A function differencing the proceeds and present value used with
  # uni-root to find the spot spread to normalize discounting
  
  
  FindSpotSpread <- function(spot.spread = vector(),
                             spot.curve = vector(),
                             cash.flow = vector(),
                             time.period = vector(),
                             proceeds = numeric()){
  
    DiscRate = spot.spread + spot.curve
    Present.Value = sum((1/(1+(DiscRate)) ^ time.period) * cash.flow)
    return(proceeds - Present.Value)
  }
  
  #' A function to solve for the spread to the spot rate curve
  #'
  #' @param spot.curve A vector representing the spot rate curve
  #' @param cash.flow A vector representing the cash flow paid to the
  #' investor
  #' @param time.period A vector representing the time weights used to
  #' discount cash flow
  #' @param proceeds A numeric value proceeds paid by the investor
  SolveSpotSpread <- function(spot.curve = vector(),
                              cash.flow = vector(),
                              time.period = vector(),
                              proceeds = numeric()){
    
    spot.spread = uniroot(FindSpotSpread,
                          interval = c(-.2, .2),
                          tol = tolerance,
                          spot.curve = spot.curve,
                          cash.flow = cash.flow,
                          time.period = time.period,
                          proceeds = proceeds)$root
    return(spot.spread)}
  
  #' @title Spread To Price MBS
  #' @family Pricing
  #' @description Returns the clean price of the MBS given a prepayment speed
  #' and spread to the curve
  #' @importFrom utils capture.output
  #' @param bond.id A character string the cusip number or bond.id
  #' @param settlement.date A character string the settlement date mm-dd-YYYY
  #' @param rates.data A character referencing a rates.data object
  #' @param prepayment.assumption A character string referencing an object of Prepayment
  #' @param spread A charcter string the spread to the interpolated curve
  #' entered in basis points
  #' @param benchmark Optionally the user can pass pricing benchmark - currently 
  #' this functonality is not implemented.
  #' @param ... Optional values follow
  #' @param CPR A numeric value the CPR assumption used to price the MBS.  For
  #' example 16 CPR is entered 16.
  #'@importFrom splines interpSpline
  #'@importFrom stats predict
  #'@importFrom stats uniroot
  #'@export
  SpreadToPriceMBS <- function(bond.id,
                               rates.data,
                               settlement.date,
                               prepayment.assumption,
                               spread,
                               benchmark = NULL,
                               ...,
                               CPR = NULL
                               ){
    
    Spread <- SpreadTypes(spread = spread)
    
    Burnout = BurnOut(bond.id)
    orig.bal = OriginalBal(bond.id)
    principal = orig.bal * MBSFactor(bond.id)
    frequency = Frequency(bond.id)

    issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
    start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
    end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
    lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    coupon = Coupon(bond.id)
    frequency = Frequency(bond.id)
    delay = PaymentDelay(bond.id)
    settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
    bondbasis = BondBasis(bond.id)

  
    # Basis spline to interpolate given yield curve
    ModelCurve <- splines::interpSpline(as.numeric(rates.data[2,2:12]),
                                        as.numeric(rates.data[1,2:12]),
                                        bSpline = TRUE)
    
    
    MBS.CF.Table = CashFlowEngine(
      bond.id = bond.id,
      settlement.date = settlement.date,
      principal = principal,
      PrepaymentAssumption = prepayment.assumption)
    
    Factor = BondBasisConversion(
      issue.date = issue.date,
      start.date = start.date,
      end.date = end.date,
      settlement.date = settlement.date,
      lastpmt.date = lastpmt.date,
      nextpmt.date = nextpmt.date,
      type = bondbasis)
    accrued.interest = Factor * as.numeric(MBS.CF.Table[1,"Pass Through Interest"])
    
    
    # Weighted Average Life
    WAL = sum(((MBS.CF.Table[,"Scheduled Prin"] +
                  MBS.CF.Table[,"Prepaid Prin"] +
                  MBS.CF.Table[,"Recovered Amount"]) *
                 MBS.CF.Table[,"Time"])/
                sum(MBS.CF.Table[,"Scheduled Prin"] +
                      MBS.CF.Table[,"Prepaid Prin"] +
                      MBS.CF.Table[,"Recovered Amount"]))
    
    # use predict ModelCurve to determine interpolated value of Icurve
    ICurve = predict(ModelCurve, WAL)$y
    
    YieldTypes <- YieldTypes(yield = (ICurve + SpreadDecimal(Spread)))
    
    # Present value of the cash flows Present Value Factors
    MBS.CF.Table[,"Present Value Factor"] =
      1/((1+(YieldBasis(YieldTypes)/frequency))^(MBS.CF.Table[,"Time"] * frequency))
    
    # Present Value of the cash flows
    MBS.CF.Table[,"Present Value"] =
      MBS.CF.Table[,"Investor CashFlow"] *
      MBS.CF.Table[,"Present Value Factor"]
    
    price = ((sum(MBS.CF.Table[,"Present Value"]) - accrued.interest) / principal) * price.basis
    PriceTypes <- PriceTypes(price = as.character(price))
    return(PriceTypes)
  }
  
  #'@title Spread to Price Bond
  #'@family Pricing
  #'@description returns the clean price of a bond given via PriceTypes object 
  #'given a spread and benchmark. market convention is to quote spread to a 
  #'benchmark in basis points.  Bond Lab follows the market convection.  
  #'The user specified spread to the benchmark in basis points.
  #'@param bond.id a character or connection referencing an object of type BondDetails
  #'@param rates.data a character the referencing a curve object
  #'@param settlement.date a character the settlement.date
  #'@param spread the spread to the benchmark given in basis points.
  #'@param benchmark the pricing benchmark.  The default is NULL in which case the
  #'function will determine the nearest pricing benchmark.  The user override values are
  #'1, 2, 3, 4, 5, 7, 10, 30
  #'@export SpreadToPriceBond
  SpreadToPriceBond <- function(bond.id,
                                rates.data,
                                settlement.date,
                                spread,
                                benchmark = NULL){

    issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
    start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
    end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
    lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    coupon = Coupon(bond.id)
    frequency = Frequency(bond.id)
    settlement.date = as.Date(settlement.date, "%m-%d-%Y")
    bondbasis = BondBasis(bond.id)
    principal = OfferAmount(bond.id)
    
    if(grepl('ActualActual', BondBasis(bond.id)) == TRUE | grepl('Actual365', BondBasis(bond.id)) == TRUE){
      days.in.year = days.in.year} else {days.in.year = days.in.year.360}
    
    Spreads <- SpreadTypes(spread = spread)
    
    ModelCurve <- splines::interpSpline(as.numeric(rates.data[2,2:12]),
                                        as.numeric(rates.data[1,2:12]),
                                        bSpline = TRUE)
    
    trade.date = as.Date(rates.data[1,1])
    
    maturity.date = as.Date(Maturity(bond.id), format = '%m-%d-%Y')
    maturity.years = as.numeric(difftime(maturity.date, trade.date)/days.in.year)
    
    if(is.null(benchmark) == TRUE){
      # Find the cloest maturity for spread to benchmark
      RatesIndex =  which(round(abs(as.numeric(rates.data[2,2:12])-
                                maturity.years - .0001),3) ==
                            min(round(abs(as.numeric(rates.data[2,2:12])-
                                      maturity.years - .0001),3))
                          )
    } else {RatesIndex = which(abs(as.numeric(rates.data[2,2:12])-
                                     benchmark) ==
                                 min(abs(as.numeric(rates.data[2,2:12])-
                                           benchmark)))}
    
    benchmark = as.numeric(rates.data[2,RatesIndex + 1])
    benchmark.yield = as.numeric(rates.data[1,RatesIndex + 1])
    
    #use predict ModelCurve to determine interpolated value of curve
    ICurve = predict(ModelCurve, benchmark)$y
    YieldTypes <- YieldTypes( yield = (ICurve + SpreadDecimal(Spreads)))
    
    Bond.CF.Table <- CashFlowBond(bond.id = bond.id,
                                  principal = principal,
                                  settlement.date = settlement.date)

    Factor = BondBasisConversion(
      issue.date = issue.date,
      start.date = NULL,
      end.date = end.date,
      settlement.date = settlement.date,
      lastpmt.date = lastpmt.date,
      nextpmt.date = nextpmt.date,
      type = bondbasis)

    accrued.interest = Factor * as.numeric(Bond.CF.Table[1, "Coupon Income"])
    
    Bond.CF.Table[,"Present Value Factor"] =
      1/((1+(YieldBasis(YieldTypes)/frequency))^(Bond.CF.Table[,"Time"] * frequency))
    
    # Present Value of the cash flows
    Bond.CF.Table[,"Present Value"] = Bond.CF.Table[,"TotalCashFlow"] * Bond.CF.Table[,"Present Value Factor"]
    price = ((sum(Bond.CF.Table[,"Present Value"])- accrued.interest) / principal) * price.basis
    PriceTypes <- PriceTypes(price = as.character(price))
    
    return(PriceTypes)
  }
  
  #'@title ZV Spread to Price Bond
  #'@family Pricing
  #'@description returns the clean price of a bond via PriceTypes object given a 
  #'spread to the spot rate curve.  Market convention is to quote spread to a 
  #'the spot rate curve in basis points.  Bond Lab follows the market convection.  The 
  #'user specified spread to the benchmark in basis points.
  #'@param bond.id a character or connection to object of type BondDetails
  #'@param settlement.date a character the settlement date 'mm-dd-yyyy'
  #'@param term.structure a character string referencing a term structure object
  #'@param ZV.spread a character the spread to the spot rate curve quoted in basis points
  #'@importFrom splines interpSpline
  #'@importFrom stats predict
  #'@importFrom stats uniroot
  #'@export ZVSpreadToPriceBond
  ZVSpreadToPriceBond <- function(bond.id,
                                  settlement.date,
                                  term.structure,
                                  ZV.spread){
    
    issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
    start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
    end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
    lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    coupon = Coupon(bond.id)
    frequency = Frequency(bond.id)
    settlement.date = as.Date(settlement.date, "%m-%d-%Y")
    bondbasis = BondBasis(bond.id)
    principal = OfferAmount(bond.id)
    
    if(grepl('ActualActual', BondBasis(bond.id)) == TRUE | grepl('Actual365', BondBasis(bond.id)) == TRUE){
    days.in.year = days.in.year} else {days.in.year = days.in.year.360}
    
    ZVSpread <- SpreadTypes(spread = ZV.spread)
    
    Bond.CF.Table <- CashFlowBond(bond.id = bond.id, 
                                  principal = OfferAmount(bond.id), 
                                  settlement.date = settlement.date)
    
    Factor = BondBasisConversion(
      issue.date = issue.date, 
      start.date = NULL, 
      end.date = end.date,
      settlement.date = settlement.date, 
      lastpmt.date = lastpmt.date, 
      nextpmt.date = nextpmt.date, 
      type = BondBasis(bond.id))
    
    accrued.interest = Factor * as.numeric(Bond.CF.Table[1, "Coupon Income"])

    ModelSpotCurve <- splines::interpSpline(SpotRate(term.structure)~TimePeriod(term.structure), 
                                            bSpline = TRUE)
    
    spot.rate <- predict(ModelSpotCurve, Bond.CF.Table[,'Time'])
    presentvalue = (sum(Bond.CF.Table[,'TotalCashFlow'] * 
                          (1 + (spot.rate$y/yield.basis) + (SpreadDecimal(ZVSpread)/yield.basis)) ^ 
                          -spot.rate$x)) - accrued.interest
    
    price = presentvalue/ OfferAmount(bond.id)
    price = price * price.basis
    price = PriceTypes(as.character(price))
    return(price)
  }
  
  #'@title ZV Spread to price MBS
  #'@family Pricing
  #'@description Returns the clean price of an MBS pass throughv via PriceTypes 
  #'object given a spread to the spot rate curve.  Market convention is to quote 
  #'spread to the spot rate curve in basis points.  Bond Lab follows the market 
  #'convection.  The user specified spread to the benchmark in basis points.
  #'@param bond.id a character or connection to object of type BondDetails
  #'@param settlement.date a character the settlement date 'mm-dd-yyyy'
  #'@param term.structure a character string referencing an object of type rates data
  #'@param prepayment.assumption a character string referencing an object of type Prepayment 
  #'@param ZV.spread a character the spread to the spot rate curve quoted in basis points
  #'@param ..., optional values follow
  #'@param CPR The user may specify a CPR to over riding the prepayment model
  #'@importFrom splines interpSpline
  #'@importFrom stats predict
  #'@importFrom stats uniroot
  #'@export ZVSpreadToPriceMBS
  ZVSpreadToPriceMBS <- function(bond.id,
                                 settlement.date,
                                 term.structure,
                                 prepayment.assumption,
                                 ZV.spread,
                                 ...,
                                 CPR = NULL){
    
    issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
    start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
    end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
    lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    coupon = Coupon(bond.id)
    frequency = Frequency(bond.id)
    settlement.date = as.Date(settlement.date, "%m-%d-%Y")
    bondbasis = BondBasis(bond.id)
    original.bal = OriginalBal(bond.id)
    mbs.factor = MBSFactor(bond.id)
    
    if(grepl('ActualActual', BondBasis(bond.id)) == TRUE | grepl('Actual365', BondBasis(bond.id)) == TRUE){
      days.in.year = days.in.year} else {days.in.year = days.in.year.360}
    
    ZVSpread <- SpreadTypes(spread = ZV.spread)
    
    Mtge.CF.Table <- CashFlowEngine(bond.id = bond.id,
                                    settlement.date = settlement.date,
                                    principal = original.bal * mbs.factor,
                                    PrepaymentAssumption = prepayment.assumption)
    
    Factor = BondBasisConversion(
      issue.date = issue.date, 
      start.date = NULL, 
      end.date = end.date,
      settlement.date = settlement.date, 
      lastpmt.date = lastpmt.date, 
      nextpmt.date = nextpmt.date, 
      type = BondBasis(bond.id))
    
    accrued.interest = Factor * as.numeric(Mtge.CF.Table[1, "Pass Through Interest"])
    
    ModelSpotCurve <- splines::interpSpline(SpotRate(term.structure)~TimePeriod(term.structure), 
                                            bSpline = TRUE)
    
    spot.rate <- predict(ModelSpotCurve, Mtge.CF.Table[,'Time'])
    presentvalue = (sum(Mtge.CF.Table[,'Investor CashFlow'] * 
                          (1 + (spot.rate$y/yield.basis) + (SpreadDecimal(ZVSpread)/yield.basis)) ^ 
                          -spot.rate$x)) - accrued.interest
    
    price = presentvalue/ (original.bal * mbs.factor)
    price = price * price.basis
    PriceTypes <- PriceTypes(price = as.character(price))
    return(PriceTypes)
  }
  
