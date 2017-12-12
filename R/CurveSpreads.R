
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

  
  #'@title CurveSpreads
  #'@family Pricing
  #'@description A class representing the curve spreads used to determine relative
  #'value in the fixed income markets.
  #' 
  #' @slot BenchMark The maturity of the nearest maturity on the coupon pricing 
  #' curve.
  #' @slot SpreadToBenchmark The yield (nominal) spread of the bond over the nearest
  #' maturity point along the coupon pricing curve.
  #' @slot SpreadToCurve The yield spread over the interpolated point on the 
  #' coupon curve matching the bond's weighted average life.
  #' @slot ZeroVolSpread The spread over the spot rate curve.  By market convention
  #' this is referred to as the ZeroVolSpread, an output of the OAS model, outside an
  #' OAS framework a more accurate reference to the measure is spread to the spot curve.
  #' @exportClass CurveSpreads
  setClass("CurveSpreads",
           representation(
             BenchMark = "numeric",
             SpreadToBenchmark = "numeric",
             SpreadToCurve = "numeric",
             ZeroVolSpread = "numeric"
           ))


  #' A standard generic function to access BenchMark
  #' 
  #' @param object An object of the type CurveSpreads
  #' @export BenchMark
  setGeneric("BenchMark", function(object)
    {standardGeneric("BenchMark")})
  
  #' A standard generic function to access SpreadToBenchmark
  #'
  #' @param object An object of the type CurveSpreads
  #' @export SpreadToBenchmark
  setGeneric("SpreadToBenchmark", function(object)
    {standardGeneric("SpreadToBenchmark")})

  #' A standard generic function to access SpreadToCurve
  #'
  #' @param object An object of the type SpreadToCurve
  #' @export SpreadToCurve
  setGeneric("SpreadToCurve", function(object)
    {standardGeneric("SpreadToCurve")})
  
  #' A standard generic function to access ZeroVol Spread
  #' 
  #' @param object An object of the type SpreadToCurve
  #' @export ZeroVolSpread
  setGeneric("ZeroVolSpread", function(object)
    {standardGeneric("ZeroVolSpread")})

  setMethod("initialize",
            signature("CurveSpreads"),
            function(.Object,
                     BenchMark = numeric(),
                     SpreadToBenchmark = numeric(),
                     SpreadToCurve = numeric(),
                     ZeroVolSpread = numeric(),
                     ...){
              callNextMethod(.Object,
                             BenchMark = BenchMark,
                             SpreadToBenchmark = SpreadToBenchmark,
                             SpreadToCurve = SpreadToCurve,
                             ZeroVolSpread = ZeroVolSpread,
                             ...)
            })
  #'@title BenchMark (Pricing)
  #'@family Pricing
  #'@description Get the pricing benchmark.  The pricing benchmark is the nearest
  #'marutity along the pricing curve to the weighted average life of the bond.  
  #'@param object An object of the type CurveSpreads
  #' @exportMethod BenchMark
  setMethod("BenchMark", signature("CurveSpreads"),
            function(object){object@BenchMark})
  
  #'@title Spread To Benchmark
  #'@family Pricing
  #'@description The bond yield (nominal) spread relative to that of the nearest
  #'pricing benchmark along the curve.
  #'@param object An object of type CurveSpreads
  #'@exportMethod SpreadToBenchmark
  setMethod("SpreadToBenchmark", signature("CurveSpreads"),
            function(object){object@SpreadToBenchmark})

  #'@title Spread To curve
  #'@family Pricing
  #'@description The bond yield spread (nominal) relative to the interpolated
  #'point along the couopn curve matching the weighted average life. 
  #'@param object An object of type CurveSpreads
  #'@exportMethod SpreadToCurve
  setMethod("SpreadToCurve", signature("CurveSpreads"),
            function(object){object@SpreadToCurve})
  
  #'@title Zero Volatility Spread
  #'@family Pricing
  #'@description The bond spread over the spot rate curve.  Within the OAS framework
  #'the term is correctly applied.  When calculated over a static sport rate curve
  #'spot spread is a more appropriate description.  Keeping with market convention the 
  #'term ZeroVolSpread is applied.
  #' @param object An object of type CurveSpreads
  #' @exportMethod ZeroVolSpread
  setMethod("ZeroVolSpread", signature = ("CurveSpreads"),
            function(object){object@ZeroVolSpread})

  #'@title Compute Curve Spreads
  #'@family Pricing
  #'@description Given proceeds, coupon curve, term structure, and cashflow compute
  #'the curve spreads used to determine relative value in the fixed income markets.
  #'the function returns the class CurveSpreads which contains the Benchmark, 
  #'SpreadToBenchMark, SpreadToCurve, and ZeroVolSpread.
  #'@param rates.data a character string referencing a rates.data object.
  #'@param CashFlow a character string referencing an object of type MBSCashFlow or BondCashFlow
  #'@param TermStructure a character string referencing an object of type TermStructure
  #'@param proceeds a numeric value the investor trade proceeeds.
  #'@importFrom splines interpSpline
  #'@importFrom stats predict
  #'@importFrom stats uniroot
  #'@export CurveSpreads
  CurveSpreads <- function(rates.data,
                           CashFlow,
                           TermStructure,
                           proceeds){

    # Get market curve for interpolation of nominal spread to curve
    MarketCurve <- rates.data
    RateLen <- as.numeric(ncol(MarketCurve))
    
    # Basis spline
    ModelCurve <- splines::interpSpline(as.numeric(MarketCurve[2,2:12]),
                               as.numeric(MarketCurve[1,2:12]),
                               bSpline = TRUE)

    # use predict ModelCurve to spread to curve
    SpreadToCurve <- (YieldToMaturity(CashFlow)) -
      predict(ModelCurve, WAL(CashFlow))$y

    # Find the cloest maturity for spread to benchmark
    RatesIndex =  which(abs(as.numeric(MarketCurve[2,2:12])-
                              as.numeric(WAL(CashFlow)) - .0001) ==
                          min(abs(as.numeric(MarketCurve[2,2:12])-
                                    as.numeric(WAL(CashFlow)) - .0001)))
    # BenchMark maturity
    BenchMarkMaturity <- as.numeric(MarketCurve[2,RatesIndex + 1])

    # calculate spread to benchmark
    SpreadToBenchmark <-  (YieldToMaturity(CashFlow)) -
      as.numeric(MarketCurve[1,RatesIndex + 1])
    
    # calculate ZVSpread (Spot Spread)
    #InterpolateSpot <- splines::interpSpline(
    #  difftime(as.Date(ForwardDate(TermStructure)[1:360]),
    #           TradeDate(TermStructure))/30,
    #  SpotRate(TermStructure)[1:360],
    #  bSpline = TRUE)
    
    InterpolateSpot <- splines::interpSpline(
      TimePeriod(TermStructure),
      SpotRate(TermStructure)[1:360],
      bSpline = TRUE)
    
    #SpotRates <- predict(
    #  InterpolateSpot,
    #  difftime(as.Date(PmtDate(CashFlow)),
    #            as.Date(TradeDate(TermStructure)))/30)
    
    SpotRates <- predict(
      InterpolateSpot,TimePeriod(CashFlow))

      
      FindSpread <- function(
        SpotSpread = numeric(),
        SpotRate = vector(),
        payment = vector(),
        period = vector(),
        proceeds = numeric()){
        DiscRate = SpotRate + SpotSpread
        return(proceeds - sum(1/((1+DiscRate) ^ period) * payment))}
      
      
      SpotSpread <- function(
        SpotRate = vector(),
        payment = vector(),
        period = vector(),
        proceeds = numeric()){
        tolerance = 0.00000001
        rate.basis = 100
        
        Spread <- uniroot(
          FindSpread,
          interval = c(lower = -.20, upper = .20),
          tol = tolerance,
          SpotRate = SpotRate,
          payment = payment,
          period = period,
         proceeds = proceeds)$root
        return(Spread)}

      ZVSpread <- SpotSpread(
        SpotRate = SpotRates$y/yield.basis,
        payment = TotalCashFlow(CashFlow),
        period = TimePeriod(CashFlow),
        proceeds = proceeds) * yield.basis

    new("CurveSpreads",
        BenchMark = BenchMarkMaturity,
        SpreadToBenchmark = SpreadToBenchmark,
        SpreadToCurve = SpreadToCurve,
        ZeroVolSpread = ZVSpread)
  }
