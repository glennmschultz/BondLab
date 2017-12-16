## ---- setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(cache=FALSE)
require(termstrc)
require(BondLab)
require(sqldf)
require(RSQLite)
APIKEY <- "31df8645e635484db99e5d1133ce9245"

## ---- sqlquery, echo=TRUE------------------------------------------------
  MBSData <- dbConnect(SQLite(), dbname=paste0(system.file(package = "BondLab"), "/BondData/MBSData"))
  dbGetQuery(MBSData,
             'Select Cusip
             ,Coupon
             ,AmortizationTerm
             ,price 
             from MBS')

## ---- priceclass, echo=TRUE----------------------------------------------
      cusip = "31283HY43"
      price <- dbGetQuery(MBSData, 'Select price from MBS where cusip = "31283HY43"')
      tradedate <- '05-19-2017'
      settlementdate <- '06-15-2017'
    # note PriceTypes class is used to convert price from string to
    # numeric decimal equivilant
    Price <- PriceTypes(price = as.character(price))

## ---- termstructure, echo = TRUE-----------------------------------------
   rates.data <- Rates(trade.date = tradedate)
   # note use invisible(capture.output()) to supress messages
   invisible(capture.output(
     TermStructure <- TermStructure(rates.data = rates.data, method = "dl")))

## ---- bonddata, echo=TRUE------------------------------------------------
    bond.id <- MBS(cusip = cusip)

## ---- prepayment, echo=TRUE----------------------------------------------
    MortgageRate <- MtgRate()
    ModelTune <- ModelTune(bond.id = bond.id)
    #invoke the prepayment model and assign it to object
    Prepayment <- PrepaymentModel(bond.id = bond.id,
                                  TermStructure = TermStructure,
                                  MortgageRate = MortgageRate,
                                  ModelTune = ModelTune,
                                  PrepaymentAssumption = "MODEL")

## ---- cashflow, echo=TRUE------------------------------------------------
PassThrough <-
  MortgageCashFlow(bond.id = bond.id,
                   original.bal = OriginalBal(bond.id),
                   settlement.date = settlementdate,
                   # note: here price is passed as decimal eqivalent string
                   # internally this function also uses PriceType to convert
                   # price to a numeric decimal basis
                   price = PriceDecimalString(Price),
                   PrepaymentAssumption = Prepayment)

## ---- spreads, echo=TRUE-------------------------------------------------
# curve spreads are also returned in the mortgagescenario object
# note: used getter methods on the classes to calculate proceeds
  proceeds = OriginalBal(bond.id) *MBSFactor(bond.id) * PriceBasis(Price)
# The class curve spreads calculates curve spreads for reporting
# or in this case to pass zero volatility spread to the total return function  
  CurveSpreads <- CurveSpreads(rates.data = rates.data,
                               TermStructure = TermStructure,
                               CashFlow = PassThrough,
                               proceeds = proceeds)

## ---- total return, echo=TRUE--------------------------------------------

  invisible(capture.output(
    NoChangeScenario <- MortgageScenario(
      bond.id = bond.id,
      settlement.date = settlementdate,
      rates.data = rates.data,
      price = PriceDecimalString(Price),
      original.bal = OriginalBal(bond.id),
      scenario = "NCs",
      horizon.months = 12,
      method = "ns",
      prepayment = "MODEL",
      horizon.spot.spread = ZeroVolSpread(CurveSpreads))))

## ---- return, echo=TRUE--------------------------------------------------
HorizonReturn(NoChangeScenario)
ZeroVolSpread(NoChangeScenario)
SpreadToCurve(NoChangeScenario)
SpreadToBenchmark(NoChangeScenario)
BenchMark(NoChangeScenario)
WAL(PassThrough)

## ---- MyPassThrough, echo= TRUE------------------------------------------
MyScenario <- function(bond.id = "character",
                       trade.date = "character",
                       settlement.date = "character",
                       prepayment = "character",
                       ...,
                       price = NULL,
                       spread = NULL,
                       CPR = numeric()){
  
  Price <- PriceTypes(price = price)
  bond.id <- MBS(MBS.id = bond.id)
  rates.data <- Rates(trade.date = trade.date)
  MortgageRate <- MtgRate()
  ModelTune <- ModelTune(bond.id = bond.id) 
  
  invisible(capture.output(
     TermStructure <- TermStructure(rates.data = rates.data, method = "ns")))
  
    #invoke the prepayment model and assign it to object
    Prepayment <- PrepaymentModel(bond.id = bond.id,
                                  TermStructure = TermStructure,
                                  MortgageRate = MortgageRate,
                                  ModelTune = ModelTune,
                                  PrepaymentAssumption = prepayment,
                                  CPR = CPR)
  CashFlow <-
  MortgageCashFlow(bond.id = bond.id,
                   original.bal = OriginalBal(bond.id),
                   settlement.date = settlementdate,
                   # note: here price is passed as decimal eqivalent string
                   # internally this function also uses PriceType to convert
                   # price to a numeric decimal basis
                   price = PriceDecimalString(Price),
                   PrepaymentAssumption = Prepayment)
  
  # note: used getter methods on the classes to calculate proceeds
  proceeds = OriginalBal(bond.id) *MBSFactor(bond.id) * PriceBasis(Price)
  # The class curve spreads calculates curve spreads for reporting
  # or in this case to pass zero volatility spread to the total return function  
  CurveSpreads <- CurveSpreads(rates.data = rates.data,
                               TermStructure = TermStructure,
                               CashFlow = PassThrough,
                               proceeds = proceeds)
  
    invisible(capture.output(
      Scenario <- MortgageScenario(
      bond.id = bond.id,
      settlement.date = settlementdate,
      rates.data = rates.data,
      price = PriceDecimalString(Price),
      original.bal = OriginalBal(bond.id),
      scenario = "NCs",
      horizon.months = 12,
      method = "ns",
      prepayment = "MODEL",
      horizon.spot.spread = ZeroVolSpread(CurveSpreads))))
    
    return(Scenario)

}


