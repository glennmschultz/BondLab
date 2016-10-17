
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.
  
  #' @include CurveSpreads.R MortgageKeyRate.R
  NULL
  
  #' An S4 class the Mortgage.OAS constructor function
  #' 
  #' @slot OAS A numeric value the option adjusted spread
  #' @slot OptionAdjDur A numeric value the option adjusted duration
  #' @slot OptionAdjCvx A numeric value the option adjusted convexity
  #' @slot ZeroVolSpread A numeric value the zero volatility spread
  #' @slot SpreadToCurve A numeric value the spread to the curve
  #' @slot EffDuration A numeric value the effective duration
  #' @slot EffConvexity A numeric value the effective convexity
  #' @slot KeyRateTenor A numeric value the Key Rate Tenor
  #' @slot KeyRateDuration A numeric value the Key Rate Duration
  #' @slot KeyRateConvexity A numeric value the Key Rate Convexity
  #' @slot PriceDist A numeric value the price distribution from the OAS model
  #' @slot PathSpread A numeric value the spot rate spread along each path
  #' @slot PathWAL A numeric value the WAL along each path
  #' @slot PathModDur A numeric value the modified duration along each path
  #' @slot PathYTM A numeric value the yield to maturity along each path
  #' @slot OASTermStructure A S4 object the zero volatility Term Structure
  #' @slot RatePaths A matrix the OAS Rate Paths
  #' @slot PrepaymentVectors A matrix the prepayment vectors returned along 
  #' each interest rate path
  #' @exportClass MortgageOAS 
  setClass("MortgageOAS",
         representation(
           OAS = "numeric",
           OptionAdjDur = "numeric",
           OptionAdjCvx = "numeric",
           ZeroVolSpread = "numeric",
           SpreadToCurve = "numeric",
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric",
           PriceDist = "vector",
           PathSpread = "vector",
           PathWAL = "vector",
           PathModDur = "vector",
           PathYTM = "vector",
           OASTermStructure = "TermStructure",
           RatePaths = "matrix",
           PrepaymentVectors = "matrix"))
  
  setGeneric("Mortgage.OAS",function(
    bond.id = "character",
    trade.date = "character",
    settlement.date = "character",
    original.bal = numeric(),
    price = numeric(),
    sigma = numeric(),
    paths = numeric()) 
    
  {standardGeneric("Mortgage.OAS")})
  
  #' A standard generic function to access the slot OAS
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("OAS", function(object)
  {standardGeneric("OAS")})
  
  #' A standard generic function to access the slot OptionAdjDur
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("OptionAdjDur", function(object)
    {standardGeneric("OptionAdjDur")})
  
  #' A standard generic function to access the slot OptionAdjCvx
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("OptionAdjCvx", function(object)
    {standardGeneric("OptionAdjCvx")})
  
  # Note: ZeroVolSpread is defined in CurveSpreads.R
  # Note: SpreadToCurve generic is defined in CurveSpreads.R
  # Note: EffDuration generic is defined in MortgageKeyRate.R
  # Note: EffConvexity generic is defined in MortgageKeyRate.R
  # Note: KeyRateTenor generic is defined in MortgageKeyRate.R
  # Note: KeyRateDuration generic is defined in MortgageKeyRate.R
  # Note: KeyRateConvexity generic is defined in MortgageKeyRate.R
  
  #' A standard generic function to access the slot PriceDist
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("PriceDist", function(object)
    {standardGeneric("PriceDist")})
  
  #' A standard generic function to access the slot PathSpread
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("PathSpread", function(object)
    {standardGeneric("PathSpread")})
  
  #' A standard generic function to access the slot PathWAL
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("PathWAL", function(object)
    {standardGeneric("PathWAL")})
  
  #' A standard generic function to access the slot PathModDuration
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("PathModDur", function(object)
    {setGeneric("PathModDur")})
  
  #' A standard generic function to access the slot PathYTM
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("PathYTM", function(object)
    {standardGeneric("PathYTM")})
  
  #' A standard generic function to access the slot OASTermStructure
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("OASTermStructure", function(object)
    {standardGeneric("OASTermStructure")})
  
  #' A standard generic function to access the slot RatePaths
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("RatePaths", function(object)
    {standardGeneric("RatePaths")})
  
  #' A standard generic function to access the slot PrepaymentVectors
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("PrepaymentVectors", function(object)
    {standardGeneric("PrepaymentVectors")})
  
  setMethod("initialize",
            signature("MortgageOAS"),
            function(.Object,
                     OAS = "numeric",
                     OptionAdjDur = "numeric",
                     OptionAdjCvx = "numeric",
                     ZeroVolSpread = "numeric",
                     SpreadToCurve = "numeric",
                     EffDuration = "numeric",
                     EffConvexity = "numeric",
                     KeyRateTenor = "numeric",
                     KeyRateDuration = "numeric",
                     KeyRateConvexity = "numeric",
                     PriceDist = "vector",
                     PathSpread = "vector",
                     PathWAL = "vector",
                     PathModDur = "vector",
                     PathYTM = "vector",
                     OASTermStructure = "S4",
                     RatePaths = "matrix",
                     PrepaymentVectors = "matrix"
            ){
              callNextMethod(.Object,
                             OAS = OAS,
                             OptionAdjDur = OptionAdjDur,
                             OptionAdjCvx = OptionAdjCvx,
                             ZeroVolSpread = ZeroVolSpread,
                             SpreadToCurve = SpreadToCurve,
                             EffDuration = EffDuration,
                             EffConvexity = EffConvexity,
                             KeyRateTenor = KeyRateTenor,
                             KeyRateDuration = KeyRateDuration,
                             KeyRateConvexity = KeyRateConvexity,
                             PriceDist = PriceDist,
                             PathSpread = PathSpread,
                             PathWAL = PathWAL,
                             PathModDur = PathModDur,
                             PathYTM = PathYTM,
                             OASTermStructure = OASTermStructure,
                             RatePaths = RatePaths,
                             PrepaymentVectors = PrepaymentVectors)
            })
  
  #' A method to extract OAS from S4 object MortgageOAS
  #' 
  #' @param object An S4 object of the type MortgageOAS
  #' @exportMethod OAS
  setMethod("OAS", signature("MortgageOAS"),
            function(object){
              object@OAS
            })
  
  #' A method to extract the OptionAdjDur from S4 object MortgageOAS
  #' 
  #' @param object An S4 object of the type MortgageOAS
  #' @export OptionAdjDur
  setMethod("OptionAdjDur", signature("MortgageOAS"),
            function(object){
              object@OptionAdjDur
            })
  #' A method to extract the OptionAdjDur from S4 object MortgageOAS
  #' 
  #' @param object An S4 object of the type MortgageOAS
  #' @export OptionAdjCvx
  setMethod("OptionAdjCvx", signature("MortgageOAS"),
            function(object){
              object@OptionAdjCvx
            })
  
  #' A method to extract ZeroVolSpread from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod ZeroVolSpread
  setMethod("ZeroVolSpread", signature("MortgageOAS"),
            function(object){
              object@ZeroVolSpread
            })
  
  #' A method to extract SpreadToCurve from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod SpreadToCurve
  setMethod("SpreadToCurve", signature("MortgageOAS"),
            function(object){
              object@SpreadToCurve})
  
  #' A method to extract EffDuration from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod EffDuration
  setMethod("EffDuration", signature("MortgageOAS"),
            function(object){
              object@EffDuration
            })
  
  #' A method to extract EffConvexity from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod EffConvexity
  setMethod("EffConvexity", signature("MortgageOAS"),
            function(object){
              object@EffConvexity
            })
  
  #' A method to extract KeyRateTenor from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the typre MortgageOAS
  #' @exportMethod KeyRateTenor
  setMethod("KeyRateTenor", signature("MortgageOAS"),
            function(object){
              object@KeyRateTenor
            })
  
  #' A method to extract KeyRateDuration from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod KeyRateDuration
  setMethod("KeyRateDuration", signature("MortgageOAS"),
            function(object){
              object@KeyRateDuration
            })
  
  #' A method to extract KeyRateConvexity from S4 object MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod KeyRateConvexity
  setMethod("KeyRateConvexity", signature("MortgageOAS"),
            function(object){
              object@KeyRateConvexity
            })
  
  #' A method to extract PriceDist from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod PriceDist
  setMethod("PriceDist", signature("MortgageOAS"),
            function(object){
              object@PriceDist
            })
  
  #' A method to extract PathSpread from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod PathSpread
  setMethod("PathSpread", signature("MortgageOAS"),
            function(object){
              object@PathSpread
            })
  
  #' A method to extract PathWAL from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS 
  #' @exportMethod PathWAL
  setMethod("PathWAL", signature("MortgageOAS"),
            function(object){
              object@PathWAL
            })
  
  #' A method to extract PathModDur from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod PathModDur
  setMethod("PathModDur", signature("MortgageOAS"),
            function(object){
              object@PathModDur
            })
  
  #' A method to extract PathYTM from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod PathYTM
  setMethod("PathYTM", signature("MortgageOAS"),
            function(object){
              object@PathYTM
            })
  
  #' A method to extract OASTermStructure from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod OASTermStructure
  setMethod("OASTermStructure", signature("MortgageOAS"),
            function(object){
              object@OASTermStructure
            })
  
  #' A method to extact RatePaths from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod RatePaths
  setMethod("RatePaths", signature("MortgageOAS"),
            function(object){
              object@RatePaths
            })
  
  #' A method to extract PrepaymentVectors from S4 class MortgageOAS
  #' 
  #' @param object an S4 object of the type MortgageOAS
  #' @exportMethod PrepaymentVectors
  setMethod("PrepaymentVectors", signature("MortgageOAS"),
            function(object){
              object@PrepaymentVectors
            })

  #' Mortgage OAS the OAS engine for pass through OAS
  #' 
  #' Pass through OAS engine
  #' @param bond.id A character string the bond id or cusip number
  #' @param trade.date A character string the trade date
  #' @param settlement.date A character string the settlment date
  #' @param original.bal A numeric value the pool original balance
  #' @param price A numeric value the price of the pass through
  #' @param sigma A numeric value the volatility assumption (not annualized)
  #' @param paths A numeric value the number of paths
  #' @importFrom lubridate %m+%
  #' @export Mortgage.OAS
  Mortgage.OAS <- function(
    bond.id = "character", 
    trade.date = "character", 
    settlement.date = "character", 
    original.bal = numeric(),
    price = "character", 
    sigma = numeric(), 
    paths = numeric()
    ){

  # The first step is to read in the Bond Detail, rates, and 
  # Prepayment Model Tuning Parameters
  bond.id = MBS(MBS.id = bond.id)
  
  # Establish connection to mortgage rate model
  MortgageRate <- MtgRate()
  
  # Establish connection to prepayment model tuning parameter
  ModelTune <- ModelTune(bond.id = bond.id)
  
  # Call the desired curve from rates data folder
  rates.data <- Rates(trade.date = trade.date)
  
  # PriceType class to convert price
  Price <- PriceTypes(Price = price)
  
  issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
  start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
  end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
  lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
  nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
  coupon = Coupon(bond.id)
  frequency = Frequency(bond.id)
  delay = PaymentDelay(bond.id)
  factor = MBSFactor(bond.id)
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  principal = original.bal * factor
  bondbasis = BondBasis(bond.id)
  Burnout = BurnOut(bond.id)
  
  short.rate = as.numeric(rates.data[1,2])/yield.basis
  
  # The spot spread function is used to solve for the spread 
  # to the spot curve to normalize discounting
  # This function is encapasulated in term structure
  
  Spot.Spread <- function(
    spread = numeric(),
    cashflow = vector(),
    discount.rates = vector(),
    t.period = vector(),
    proceeds = numeric()){
    
  Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
                return(proceeds - Present.Value)
  }
  
  # First, calibrate the interest rate model to market swap rates and prices
  # Set trade date and call the CalibrateCIR Model
  # trade.date = as.Date(trade.date, "%m-%d-%Y")
  
  Market.Fit <- CalibrateCIR(trade.date = trade.date, 
                             sigma = sigma)
  kappa  = Market.Fit$p1
  lambda = Market.Fit$p2
  theta  = Market.Fit$p3

  # Calculate the number of cashflows that will be paid from settlement date 
  # to the last pmt date (used end date as next pmdt date for this)
  ncashflows = BondBasisConversion(
    issue.date = issue.date,
    start.date = start.date,
    end.date = end.date,
    settlement.date = settlement.date,
    lastpmt.date = lastpmt.date,
    nextpmt.date = end.date,
    type = bondbasis) 
  
  # Build a vector of dates for the payment schedule
  # first get the pmtdate interval
  pmtdate.interval = months.in.year/frequency
  
  # Compute the payment dates 
  pmtdate = as.Date(c(if(
    settlement.date == issue.date) 
    {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))
    } else {seq(nextpmt.date, end.date, 
                by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y") + delay
  
  # Build the time period vector (n) for discounting the cashflows nextpmt date 
  # is vector of payment dates to n for each period
  time.period <- BondBasisConversion(
    issue.date = issue.date,
    start.date = start.date,
    end.date = end.date,
    settlement.date = settlement.date,
    lastpmt.date = lastpmt.date,
    nextpmt.date = pmtdate,
    type = bondbasis)
  
  # step4 Count the number of cashflows 
  # num.periods is the total number of cashflows to be received
  # num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  num.period = seq(1:num.periods)

  # ==== Compute Option Adjusted Spread ======================================
  # For simulation pass T = mortgage term if the number of paths = 1 
  # then volatility = 0
  set.seed(300) 
  Simulation <- CIRSim(
    shortrate = short.rate,
    kappa = kappa,
    theta = theta,
    T = ((num.periods-1) / months.in.year),
    step = (1/months.in.year),
    sigma = sigma,
    N = paths)
  
  SimulationUp <- CIRSim(
    shortrate = short.rate + (rate.delta/yield.basis),
    kappa = kappa,
    theta = theta,
    T = ((num.periods-1) / months.in.year),
    step = (1/months.in.year),
    sigma = sigma,
    N = paths)
  
  SimulationDwn <- CIRSim(
    shortrate = short.rate - (rate.delta/yield.basis),
    kappa = kappa,
    theta = theta,
    T = ((num.periods-1) / months.in.year),
    step = (1/months.in.year),
    sigma = sigma,
    N = paths)

  # number of rows in the simulation will size the arrays
  num.sim <- nrow(Simulation)

  # Dim arrays for the calculation
  cube.names <- c("Period", 
                  "Date", 
                  "Time", 
                  "SpotRate", 
                  "DiscRate", 
                  "TwoYear", 
                  "TenYear")    
  
  sim.cube <- array(data = NA, c(num.sim, 7), 
                    dimnames = list(seq(c(1:num.sim)),cube.names))
  
  # Populate the simulation cube  
  sim.cube[,1] <- num.period
  sim.cube[,2] <- pmtdate
  sim.cube[,3] <- time.period
  
  # Dimension the arrays that will be needed
  oas.names <- c("OAS", 
                 "WAL", 
                 "ModDur", 
                 "YTM", 
                 "Price")
  
  # OAS out holds OAS solutions to individual trajectory calculations 
  # solving for the spread to price
  OAS.Out <- array(data = NA, c(paths,5), 
                   dimnames = list(seq(c(1:paths)),oas.names))
  
  OAS.CashFlow <- array(data = NA, c(num.sim,paths))
  OAS.DiscMatrix <- array(data = NA, c(num.sim, paths))
  prepayout <- NULL #array(data = NA, c(num.sim, paths))
  
  #Initialize empty TermStructure class.  If one repeatedly uses new term
  #structure in the loop the memory footprint will grow with the number of loops
  
  OAS.Term.Structure <- new("TermStructure",
                            TradeDate = as.character(trade.date),
                            Period = numeric(),
                            Date = "character",
                            SpotRate = numeric(),
                            ForwardRate = numeric(),
                            TwoYearFwd = numeric(),
                            TenYearFwd = numeric()
  ) 
  
  for(j in 1:paths){

  
  # This is an attempt to compute the forward curve for the path from the simulated
  # short term rates  
  sim.cube[,4] <- cumprod(1 + Simulation[,j])
    
  #sim.cube 5 is the spot rates implied from the forward rate curve above
  #sim.cube[,5] <- (((sim.cube[,4] ^ (1/ sim.cube[,3]))^(1/months.in.year))-1)
  
  # Note sim.cube[5], sim.cube[,6] and sim.cube[,7] are multiplied by yield 
  # basis to adjust the values to pass into the mortgage rate function of the 
  # prepayment model Note sim.cube[,6] and sim.cube[,7] are multiplied by yield 
  # basis to adjust the values to pass into the mortgage rate function of the 
  # prepayment model
  
  sim.cube[,5] <- as.vector(CIRBondPrice(
    shortrate = as.numeric(Simulation[, j]), 
    kappa = kappa, 
    lambda = lambda, 
    theta = theta, 
    sigma = sigma, 
    T = 1/12, 
    step = 0, 
    result = "y") * yield.basis)
  
  sim.cube[,6] <- as.vector(CIRBondPrice(
    shortrate = as.numeric(Simulation[, j]), 
    kappa = kappa, 
    lambda = lambda, 
    theta = theta, 
    sigma = sigma, 
    T = 2, 
    step = 0, 
    result = "y") * yield.basis)
    
    sim.cube[,7] <- as.vector(CIRBondPrice(
      shortrate = as.numeric(Simulation[, j]), 
      kappa = kappa, 
      lambda = lambda, 
      theta = theta, 
      sigma = sigma, 
      T = 10, 
      step = 0, 
      result = "y") * yield.basis)
    
  #Initialize OAS Term Structure object.  This object is passed to 
  #prepayment assumption Allows the prepayment model to work in the 
  #Option Adjusted Spread function replacing Term Structure
  #When sigma is zero the simulated spot rates are compounded forward 
  #rates and the two and ten year rates are calcualted from the calculated 
  #spot rate rate curve
  
 
  # Use TermStructure setter
  Period(OAS.Term.Structure) <- as.numeric(sim.cube[,3])
  ForwardDate(OAS.Term.Structure) <- unname(
  as.character(as.Date(sim.cube[,2], origin = "1970-01-01")))
  SpotRate(OAS.Term.Structure) <- as.numeric(Simulation[,j])
  ForwardRate(OAS.Term.Structure) <- as.numeric(Simulation[,4])
  TwoYearForward(OAS.Term.Structure) <- as.numeric(sim.cube[,6])
  TenYearForward(OAS.Term.Structure) <- as.numeric(sim.cube[,7])

  Prepayment <- PrepaymentModel(
    bond.id = bond.id,
    TermStructure = OAS.Term.Structure,
    MortgageRate = MortgageRate,
    PrepaymentAssumption = "MODEL",
    ModelTune = ModelTune,
    Burnout = Burnout,
    Severity = 0)


  # Assign the prepayment vector to the prepayment out matrix
   prepayout <- cbind(prepayout, Prepayment@SMM)


  MtgCashFlow <- MortgageCashFlow(
    bond.id = bond.id,
    original.bal = original.bal,
    settlement.date = settlement.date,
    price = PriceDecimalString(Price),
    PrepaymentAssumption = Prepayment)
    
    OAS.CashFlow[,j] <- as.vector(MtgCashFlow@TotalCashFlow)
    OAS.DiscMatrix [,j] <- as.numeric(Simulation[,j]) #as.vector(sim.cube[,5])

    #This needs some error trapping on price
    proceeds <- as.numeric((
      original.bal * 
        factor * PriceBasis(Price)) + 
        MtgCashFlow@Accrued)
    
    curr.bal <- as.numeric(original.bal * factor)
    
    #Solve for spread to spot curve to equal price 
    OAS.Out[j,1] <- uniroot(Spot.Spread, 
                            interval = c(-1, 1), 
                            tol = .0000000001, 
                            cashflow = MtgCashFlow@TotalCashFlow,
                            discount.rates = OAS.Term.Structure@SpotRate, 
                            t.period = OAS.Term.Structure@Period, 
                            proceeds)$root
    
    OAS.Out[j,2] <- MtgCashFlow@WAL
    OAS.Out[j,3] <- MtgCashFlow@ModDuration
    OAS.Out[j,4] <- MtgCashFlow@YieldToMaturity
    
  } # end of the OAS j loop
  
  # Assign the prepayment and rate vectors to matrix for assignment
  # MortgageOAS slot 
   vectors <- prepayout
   paths <- Simulation

   OAS.Spread <- mean(OAS.Out[,1])
  
  #
  #Calculate OAS to price for price distribution analysis
  OAS.Price <- function(spread = numeric(), 
                        DiscountMatrix = matrix(), 
                        CashFlowMatrix = matrix(), 
                        period = vector(), 
                        proceeds = numeric(), 
                        paths = numeric()) {
    
  OAS.Proceeds <- data.frame(
    ((1/((1 + DiscountMatrix[,] + spread)^ period)) * CashFlowMatrix[,]))
    OAS.Proceeds <- (colSums(OAS.Proceeds)/curr.bal) * price.basis
    return(OAS.Proceeds)
    }
  
  Price.Dist <- OAS.Price(OAS.Spread, 
                          DiscountMatrix = OAS.DiscMatrix, 
                          CashFlowMatrix = OAS.CashFlow,
                          period = OAS.Term.Structure@Period, 
                          proceeds = proceeds, 
                          paths = paths)
  
  OAS.Out[,5] <- Price.Dist 
  
  PriceUp <- mean(OAS.Price(OAS.Spread, 
                       DiscountMatrix = SimulationUp, 
                       CashFlowMatrix = OAS.CashFlow,
                       period = OAS.Term.Structure@Period, 
                       proceeds = proceeds, 
                       paths = paths))
  
  PriceDwn <- mean(OAS.Price(OAS.Spread, 
                       DiscountMatrix = SimulationDwn, 
                       CashFlowMatrix = OAS.CashFlow,
                       period = OAS.Term.Structure@Period, 
                       proceeds = proceeds, 
                       paths = paths))
  
  OptionAdjDur = (
    (PriceUp - PriceDwn)/(2 *PriceDecimal(Price) * (rate.delta/yield.basis))
  )
  
  OptionAdjCvx = (
    (PriceUp + PriceDwn - 2*PriceDecimal(Price))/
      (2 *PriceDecimal(Price) * (rate.delta/yield.basis)^2)
  )
  # --------------------------------------------------------------------------
  # Calculate static cash flow spread to the curve at zero volatility
  # Using the prepayment model this will always match the ZV spread indiciating
  # the pricing benchmark is exact
  # In reality the spread to the curve will be based on the pricing speed used.
  # This is a good check
  # ---------------------------------------------------------------------------
                                  
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                              as.numeric(rates.data[2,2:12]), 
                              data = data.frame(rates.data))  
    
    SpreadtoCurve = ((MtgCashFlow@YieldToMaturity) - 
                       predict(InterpolateCurve, MtgCashFlow@WAL ))
    
  OAS <- OAS.Out
  # ------------------------------------------------------------------------
  # Key Rate Duration 
  # -------------------------------------------------------------------------
    
  # CIR Bond Price returns the spot rate curve
  
    CIRFwd <- CIRSim(
      shortrate = short.rate,
      kappa = kappa,
      theta = theta,
      T = 40,
      step = 1/months.in.year,
      sigma = 0,
      N = 1
    )
  
    Spot <- cumprod(1+(CIRFwd))
    t <- seq(1,length(Spot),1)
    Spot <- (Spot^(1/t))
    CIRSpot <- Spot - 1
    
    #CIRSpot[1] <- short.rate
    Mo1Fwd <- Forward.Rate(SpotRate.Curve = CIRSpot, FwdRate.Tenor = 1)
    TwoYrFwd <-Forward.Rate(SpotRate.Curve = CIRSpot, FwdRate.Tenor = 24)
    TenYrFwd <-Forward.Rate(SpotRate.Curve = CIRSpot, FwdRate.Tenor = 120)
    
    CIRTermStructure <- new("TermStructure",
                            TradeDate = trade.date,
                            Period = as.numeric(sim.cube[,3]), 
                            Date = unname(as.character(pmtdate)),
                            SpotRate = CIRSpot * 100,
                            ForwardRate = Mo1Fwd *100,
                            TwoYearFwd = TwoYrFwd * 100,
                            TenYearFwd = TenYrFwd * 100)
    
  PrepaymentAssumption <- PrepaymentModel(
    bond.id = bond.id,
    MortgageRate = MortgageRate,
    TermStructure = CIRTermStructure,
    PrepaymentAssumption = "MODEL",
    ModelTune = ModelTune,
    Burnout = Burnout,
    Severity = 0) 

    #The fourth step is to call the bond cusip details and calculate 
    #Bond Yield to Maturity, 
    #Duration, Convexity and CashFlow.
  MortgageCashFlow <- MortgageCashFlow(
    bond.id = bond.id,
    original.bal = original.bal,
    settlement.date = settlement.date,
    price = PriceDecimalString(Price),
    PrepaymentAssumption = PrepaymentAssumption)
    
    MortgageKeyRate <- MtgTermStructure(
      bond.id = bond.id,
      original.bal = original.bal,
      Rate.Delta = rate.delta,
      TermStructure = CIRTermStructure,
      settlement.date = settlement.date,
      principal = principal,
      price = PriceDecimalString(Price),
      cashflow = MortgageCashFlow)
    
    # ----------------------------------------------------------------------
    # Solve for the zero volatility spread
    # -----------------------------------------------------------------------
    
  cashflow.length <- length(MortgageCashFlow@TotalCashFlow)
  SpotSpread <- function(spread = numeric(), 
                           cashflow = vector(), 
                           discount.rates = vector(), 
                           t.period = vector(), 
                           proceeds = numeric()){
      
  Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
  return(proceeds - Present.Value)}
    
  SolveSpotSpread <- uniroot(
    SpotSpread,
    interval = c(-.75, .75),
    tol = tolerance,
    cashflow = MortgageCashFlow@TotalCashFlow,
    discount.rates = CIRTermStructure@SpotRate[1:cashflow.length]/yield.basis,
    t.period = MortgageCashFlow@TimePeriod,
    proceeds = proceeds)$root
    
    spot.spread <- SolveSpotSpread
    
    new("MortgageOAS",
       OAS = mean(OAS.Out[,1]) * yield.basis,
       OptionAdjDur = OptionAdjDur * -1,
       OptionAdjCvx = OptionAdjCvx * .5,
       ZeroVolSpread = spot.spread * yield.basis,
       SpreadToCurve = SpreadtoCurve,
       EffDuration = EffDuration(MortgageKeyRate),
       EffConvexity = EffConvexity(MortgageKeyRate),
       KeyRateTenor = KeyRateTenor(MortgageKeyRate),
       KeyRateDuration = KeyRateDuration(MortgageKeyRate),
       KeyRateConvexity = KeyRateConvexity(MortgageKeyRate),
       PathSpread = unname(OAS.Out[,1]),
       PathWAL = unname(OAS.Out[,2]),
       PathModDur = unname(OAS.Out[,3]),
       PathYTM = unname(OAS.Out[,4]),
       PriceDist = unname(OAS.Out[,5]),
       OASTermStructure <- CIRTermStructure,
       RatePaths = paths,
       PrepaymentVector = as.matrix(prepayout))
   }
