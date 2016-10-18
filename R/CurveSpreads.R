
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

  
  #' An S4 class to hold curve spread values
  #' 
  #' @slot BenchMark A numeric value the closest curve benchmark
  #' @slot SpreadToBenchmark A numeric value to nominal spread to the nearest
  #' pricing benchmark
  #' @slot SpreadToCurve A numeric value the nominal spread to 
  #' the interpolated curve
  #' @slot ZeroVolSpread A numeric value the Zero Volatilty spread
  #' @exportClass CurveSpreads
  #' @importFrom splines interpSpline
  setClass("CurveSpreads",
           representation(
             BenchMark = "numeric",
             SpreadToBenchmark = "numeric",
             SpreadToCurve = "numeric",
             ZeroVolSpread = "numeric"
           ))

  setGeneric("CurveSpreads", function(rates.data = "character",
                                      CashFlow = "character")
    {standardGeneric("CurveSpreads")})

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
  #' A method to extract the BenchMark from object CurveSpreads
  #' 
  #' @param object An object of the type CurveSpreads
  #' @exportMethod BenchMark
  setMethod("BenchMark", signature("CurveSpreads"),
            function(object){object@BenchMark})
  
  #' A method to extract the SpreadToBenchmark from object CurveSpreads
  #'
  #' @param object An object of type CurveSpreads
  #' @exportMethod SpreadToBenchmark
  setMethod("SpreadToBenchmark", signature("CurveSpreads"),
            function(object){object@SpreadToBenchmark})

  #' A method to extract the SpreadToCurve from object CurveSpreads
  #'
  #' @param object An object of type CurveSpreads
  #' @exportMethod SpreadToCurve
  setMethod("SpreadToCurve", signature("CurveSpreads"),
            function(object){object@SpreadToCurve})
  
  #' A method to extract the ZeroVol spread from object CurveSpreads
  #' 
  #' @param object An object of type CurveSpreads
  #' @exportMethod ZeroVolSpread
  setMethod("ZeroVolSpread", signature = ("CurveSpreads"),
            function(object){object@ZeroVolSpread})

  #' A functon to compute the curve spread metrics
  #'
  #'@param rates.data a character string the yield curve used
  #'@param CashFlow a character string the object of type MBSCashFlow
  #'@param TermStructure a character string the object of type TermStructure
  #'@param proceeds a numeric value the investor trade proceeeds 
  #'MortgageCashFlow
  #'@importFrom stats loess
  #'@importFrom stats predict
  #'@export CurveSpreads
  CurveSpreads <- function(rates.data = "character",
                           CashFlow = "character",
                           TermStructure = "character",
                           proceeds = numeric()){

    # Get market curve for interpolation of nominal spread to curve
    MarketCurve <- rates.data
    RateLen <- as.numeric(ncol(MarketCurve))
    
    # local regression smooth of market curve
    ModelCurve <- loess(as.numeric(MarketCurve[1,2:12]) ~
                          as.numeric(MarketCurve[2,2:12]),
                        data = data.frame(MarketCurve))

    # use predict ModelCurve to spread to curve
    SpreadToCurve <- (YieldToMaturity(CashFlow)) -
      predict(ModelCurve, WAL(CashFlow))

    # Find the cloest maturity for spread to benchmark
    RatesIndex =  which(abs(as.numeric(MarketCurve[2,2:12])-
                              as.numeric(WAL(CashFlow))) ==
                          min(abs(as.numeric(MarketCurve[2,2:12])-
                                    as.numeric(WAL(CashFlow)))))
    # BenchMark maturity
    BenchMarkMaturity <- as.numeric(MarketCurve[2,RatesIndex + 1])

    # calculate spread to benchmark
    SpreadToBenchmark <-  (YieldToMaturity(CashFlow)) -
      as.numeric(MarketCurve[1,RatesIndex + 1])
    
    # calculate ZVSpread (Spot Spread)
    InterpolateSpot <- splines::interpSpline(
      difftime(as.Date(ForwardDate(TermStructure)[1:360]),
               TradeDate(TermStructure))/30,
      SpotRate(TermStructure)[1:360],
      bSpline = TRUE)
    
    SpotRates <- predict(
      InterpolateSpot,
      difftime(as.Date(PmtDate(CashFlow)),
                as.Date(TradeDate(TermStructure)))/30)

      
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
