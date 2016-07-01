
  # A function to calculate the spread to benchmark and
  # the spread to the curve
  # copyright 2016 Bond Lab Technologies, LLC
  
  #' An S4 class to hold curve spread values
  #' 
  #' @slot SpreadToBenchmark A numeric value to nominal spread to the nearest
  #' pricing benchmark
  #' @slot SpreadToCurve A numeric value the nominal spread to the interpolated
  #' Curve
  #' @exportClass CurveSpreads
  setClass("CurveSpreads",
           representation(
             SpreadToBenchmark = "numeric",
             SpreadToCurve = "numeric"
           ))

  setGeneric("CurveSpreads", function(rates.data = "character",
                                      MortgageCashFlow = "character")
    {standardGeneric("CurveSpreads")})

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

  setMethod("initialize",
            signature("CurveSpreads"),
            function(.Object,
                     SpreadToBenchmark = numeric(),
                     SpreadToCurve = numeric(),
                     ...){
              callNextMethod(.Object,
                             SpreadToBenchmark = SpreadToBenchmark,
                             SpreadToCurve = SpreadToCurve,
                             ...)
            })
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

  #' A functon to compute the curve spread metrics
  #'
  #'@param rates.data A character string the trade date mm-dd-YYYY
  #'@param MortgageCashFlow A character string the for object of type
  #'MortgageCashFlow
  #'@export CurveSpreads
  CurveSpreads <- function(rates.data = "character",
                           MortgageCashFlow = "character"){

    MortgageCashFlow <- MortgageCashFlow

    # Get market curve for interpolation of nominal spread to curve
    MarketCurve <- rates.data

    # local regression smooth of market curve
    ModelCurve <- loess(as.numeric(MarketCurve[2,2:12]) ~
                          as.numeric(MarketCurve[1,2:12]),
                        data = MarketCurve)

    # use predict ModelCurve to spread to curve
    SpreadToCurve <- (YieldToMaturity(MortgageCashFlow)) -
      predict(ModelCurve, WAL(MortgageCashFlow))

    # Find the cloest maturity for spread to benchmark
    RatesIndex =  which(abs(as.numeric(MarketCurve[1,2:12])-
                              as.numeric(WAL(MortgageCashFlow))) ==
                          min(abs(as.numeric(MarketCurve[1,2:12])-
                                    as.numeric(WAL(MortgageCashFlow)))))

    # calculate spread to benchmark
    SpreadToBenchmark <-  (YieldToMaturity(MortgageCashFlow)) -
      as.numeric(MarketCurve[2,RatesIndex + 1])

    new("CurveSpreads",
        SpreadToBenchmark = SpreadToBenchmark,
        SpreadToCurve = SpreadToCurve)
  }
