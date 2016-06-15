
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # File License
  # Copyright (C) 2015  Bond Lab Technologies, Inc.
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


  setClass("MortgageOAS",
         representation(
           OAS = "numeric",
           ZVSpread = "numeric",
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
           PathYTM = "vector"))
  
  setGeneric("Mortgage.OAS",function(bond.id = "character", 
                                     trade.date = "character", 
                                     settlement.date = "character", 
                                     original.bal = numeric(),
                                     price = numeric(), 
                                     sigma = numeric(), 
                                     paths = numeric()) 
    
  {standardGeneric("Mortgage.OAS")})
  
  setMethod("initialize",
            signature("MortgageOAS"),
            function(.Object,
                     OAS = "numeric",
                     ZVSpread = "numeric",
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
                     PathYTM = "vector"
            ){
              callNextMethod(.Object,
                             OAS = OAS,
                             ZVSpread = ZVSpread,
                             SpreadToCurve = SpreadToCurve,
                             EffDuration = EffDuration,
                             EffConvexity = EffConvexity,
                             KeyRateTenor = KeyRateTenor,
                             KeyRateConvexity = KeyRateConvexity,
                             PriceDist = PriceDist,
                             PathSpread = PathSpread,
                             PathWAL = PathWAL,
                             PathModDur = PathModDur,
                             PathYTM = PathYTM)

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
  Mortgage.OAS <- function(bond.id = "character", 
                         trade.date = "character", 
                         settlement.date = "character", 
                         original.bal = numeric(),
                         price = numeric(), 
                         sigma = numeric(), 
                         paths = numeric()
                         ){

  # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
  bond.id = MBS(MBS.id = bond.id)
  
  # Establish connection to mortgage rate model
  MortgageRate <- MtgRate()
  
  # Establish connection to prepayment model tuning parameter
  ModelTune <- ModelTune(bond.id = bond.id)
  
  # Call the desired curve from rates data folder
  rates.data <- Rates(trade.date = trade.date)
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  delay = bond.id@PaymentDelay
  factor = bond.id@MBSFactor
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  principal = original.bal * factor
  bondbasis = bond.id@BondBasis
  Burnout = bond.id@Burnout
  
  short.rate = as.numeric(rates.data[1,2])/yield.basis
  
  # The spot spread function is used to solve for the spread to the spot curve to normalize discounting
  # This function is encapasulated in term structure
  
  Spot.Spread <- function(spread = numeric(), 
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
  ncashflows = BondBasisConversion(issue.date = issue.date, 
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
  pmtdate = as.Date(c(if(settlement.date == issue.date) 
                      {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
                      else 
                      {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, 
                                                              "months"))}), "%m-%d-%Y") + delay
  
  
  # Build the time period vector (n) for discounting the cashflows nextpmt date 
  # is vector of payment dates to n for each period
  time.period <- BondBasisConversion(issue.date = issue.date, 
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

  # ==== Compute Option Adjusted Spread ==========================================
  # For simulation pass T = mortgage term if the number of paths = 1 then volatility = 0 
  Simulation <- CIRSim(shortrate = short.rate, 
                       kappa = kappa, 
                       theta = theta, 
                       T = ((num.periods-1) / months.in.year), 
                       step = (1/months.in.year), 
                       sigma = sigma, 
                       N = paths)
  
  # number of rows in the simulation will size the arrays
  num.sim <- nrow(Simulation)
  
  # Dim arrays for the calculation
  cube.names <- c("Period", "Date", "Time", "SpotRate", "DiscRate", "TwoYear", "TenYear")    
  sim.cube <- array(data = NA, c(num.sim, 7), dimnames = list(seq(c(1:num.sim)),cube.names))
  
  # Populate the simulation cube  
  sim.cube[,1] <- num.period
  sim.cube[,2] <- pmtdate
  sim.cube[,3] <- time.period
  
  # Dimension the arrays that will be needed
  oas.names <- c("OAS", "WAL", "ModDur", "YTM", "Price")
  # OAS out holds OAS solutions to individual trajectory calcualtions solving for the spread to price
  OAS.Out <- array(data = NA, c(paths,5), dimnames = list(seq(c(1:paths)),oas.names))
  
  OAS.CashFlow <- array(data = NA, c(num.sim,paths))
  OAS.DiscMatrix <- array(data = NA, c(num.sim, paths))
  
  prepayout <- NULL
  for(j in 1:paths){
    
   
    # sim cube 5 ifelse synchs the CIR output to that of term strucutred for MBS cashflow 
    # analysis this needs to be fixed rates should be passed through in a common scales regardless of 
    # interest rate model  
    sim.cube[,4] <- cumprod(1 + Simulation[,j])
    
    #sim.cube 5 is the discount rate to value cash flows
    sim.cube[,5] <- (((sim.cube[,4] ^ (1/ sim.cube[,3]))^(1/months.in.year))-1)
    
    # Note sim.cube[,6] and sim.cube[,7] are multiplied by yield basis to adjust
    # their values to pass into the mortgage rate function of the prepayment model
    sim.cube[,6] <- as.vector(CIRBondPrice(shortrate = as.numeric(Simulation[, j]), 
                                           kappa = kappa, 
                                           lambda = lambda, 
                                           theta = theta, sigma = sigma, 
                                           T = 2, 
                                           step = 0, 
                                           result = "y") * yield.basis)
    
    sim.cube[,7] <- as.vector(CIRBondPrice(shortrate = as.numeric(Simulation[, j]), 
                                           kappa = kappa, 
                                           lambda = lambda, 
                                           theta = theta, 
                                           sigma = sigma, 
                                           T = 10, 
                                           step = 0, 
                                           result = "y") * yield.basis)
    
    #Initialize OAS Term Structure object.  This object is passed to prepayment assumption
    #Allows the prepayment model to work in the Option Adjusted Spread function replacing Term Structure
    #When sigma is zero the simulated spot rates are compounded forward rates and the two and ten year
    #rates are calcualted from the calculated spot rate rate curve
    
   
      OAS.Term.Structure <- new("TermStructure",
                                TradeDate = as.character(trade.date),
                                Period = as.numeric(sim.cube[,3]),
                                Date = unname(as.character(as.Date(sim.cube[,2], origin = "1970-01-01"))),
                                SpotRate = as.numeric(sim.cube[,5]),
                                ForwardRate = as.numeric(Simulation[,j]),
                                TwoYearFwd = as.numeric(sim.cube[,6]),
                                TenYearFwd = as.numeric(sim.cube[,7]))
    
    
    Prepayment <- PrepaymentModel(bond.id = bond.id, 
                                       TermStructure = OAS.Term.Structure, 
                                       MortgageRate = MortgageRate, 
                                       PrepaymentAssumption = "MODEL", 
                                       ModelTune = ModelTune, 
                                       Burnout = Burnout,
                                       Severity = 0)
    
    # ----------------------------------------------------------------------------------------
    # Uncomment this line and # 239 to get the prepayment vector from the model 
    # prepayout <- cbind(prepayout, Prepayment@SMM)
    # ----------------------------------------------------------------------------------------
    
    
    MtgCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                    original.bal = original.bal, 
                                    settlement.date = settlement.date, 
                                    price = price, 
                                    PrepaymentAssumption = Prepayment)
    
    OAS.CashFlow[,j] <- as.vector(MtgCashFlow@TotalCashFlow)
    OAS.DiscMatrix [,j] <- as.vector(sim.cube[,5])
    
    
    #This needs some error trapping on price
    proceeds <- as.numeric((original.bal * factor * price/price.basis) + MtgCashFlow@Accrued)
    curr.bal <- as.numeric(original.bal * factor)
    
    #Solve for spread to spot curve to equal price 
    OAS.Out[j,1] <- uniroot(Spot.Spread, interval = c(-1, 1), 
                            tol = .0000000001, 
                            cashflow = MtgCashFlow@TotalCashFlow,
                            discount.rates = OAS.Term.Structure@SpotRate, 
                            t.period = OAS.Term.Structure@Period, 
                            proceeds)$root
    
    OAS.Out[j,2] <- MtgCashFlow@WAL
    OAS.Out[j,3] <- MtgCashFlow@ModDuration
    OAS.Out[j,4] <- MtgCashFlow@YieldToMaturity
    
  } # end of the OAS j loop
  
  # ---------------------------------------------------------------------------------------------
  # uncomment this to get prepayment vectors
  # vectors <<- prepayout
  # cashflow <<- OAS.CashFlow
  # disc <<- OAS.DiscMatrix
  # ---------------------------------------------------------------------------------------------
  # ---------------------------------------------------------------------------------------------
  # Calculate OAS spread find the spread such that the average proceeds is equal to proceeds
  # ---------------------------------------------------------------------------------------------
  OAS <- function(spread = numeric(), 
                  DiscountMatrix = matrix(), 
                  CashFlowMatrix = matrix(), 
                  period = vector(), 
                  proceeds = numeric(),
                  price = numeric(),
                  paths = numeric()) {
    
    OAS.Proceeds <- data.frame(((1/((1 + DiscountMatrix[,] + spread)^ period)) * CashFlowMatrix[,]))
    OAS.Proceeds <- colSums(OAS.Proceeds/curr.bal) * price.basis
    return(mean(OAS.Proceeds) - price)}
  
  OAS.Spread <- uniroot(OAS, 
                        interval = c(-1,1), 
                        tol = .000000001, 
                        DiscountMatrix = OAS.DiscMatrix, 
                        CashFlowMatrix = OAS.CashFlow,
                        period = OAS.Term.Structure@Period, 
                        proceeds = proceeds,
                        price = price,
                        paths = paths)$root
  
  # -------------------------------------------------------------------------------------------------------
  #Calculate OAS to price for price distribution analysis
  # -------------------------------------------------------------------------------------------------------
  OAS.Price <- function(spread = numeric(), 
                        DiscountMatrix = matrix(), 
                        CashFlowMatrix = matrix(), 
                        period = vector(), 
                        proceeds = numeric(), 
                        paths = numeric()) {
    
    OAS.Proceeds <- data.frame(((1/((1 + OAS.DiscMatrix[,] + spread)^ period)) * OAS.CashFlow[,]))
    OAS.Proceeds <- (colSums(OAS.Proceeds)/curr.bal) * price.basis
    return(OAS.Proceeds)}
  
  Price.Dist <- OAS.Price(OAS.Spread, 
                          DiscountMatrix = OAS.DiscMatrix, 
                          CashFlowMatrix = OAS.CashFlow,
                          period = OAS.Term.Structure@Period, 
                          proceeds = proceeds, 
                          paths = paths)
  
  OAS.Out[,5] <- Price.Dist 
  
  # ------------------------------------------------------------------------------------------------------
  # Calculate static cash flow spread to the curve at zero volatility
  # Using the prepayment model this will always match the ZV spread indiciating the pricing benchmark is exact
  # In reality the spread to the curve will be based on the pricing speed used.
  # This is a good check
  # --------------------------------------------------------------------------------------------------------
                                  
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                                as.numeric(rates.data[2,2:12]), data = data.frame(rates.data))  
    
    SpreadtoCurve = ((MtgCashFlow@YieldToMaturity  * yield.basis) - 
                       predict(InterpolateCurve, MtgCashFlow@WAL ))/yield.basis
    
  OAS <- OAS.Out
   # --------------------------------------------------------------------------------------------------------
   # Key Rate Duration 
   # --------------------------------------------------------------------------------------------------------
    
   # CIR Bond Price returns the spot rate curve
  
    CIRSpot <- CIRBondPrice(shortrate = short.rate,
    T = 40,
    step = 1/months.in.year,
    kappa = kappa,
    lambda = lambda,
    theta = theta,
    sigma = sigma,
    result = "y")
    
    time.period <- seq(from = 1, to = 30, by = 1)
    time.period <- time.period/12
    timelength <- length(time.period)
    
    CIRSpot[1] <- short.rate
    Mo1Fwd <- Forward.Rate(SpotRate.Curve = CIRSpot, FwdRate.Tenor = 1)
    TwoYrFwd <-Forward.Rate(SpotRate.Curve = CIRSpot, FwdRate.Tenor = 2)
    TenYrFwd <-Forward.Rate(SpotRate.Curve = CIRSpot, FwdRate.Tenor = 10)
    
    CIRTermStructure <- new("TermStructure",
                            TradeDate = trade.date,
                            Period = time.period[2:timelength],
                            Date = unname(as.character(pmtdate)),
                            SpotRate = CIRSpot * 100,
                            ForwardRate = Mo1Fwd * 100,
                            TwoYearFwd = TwoYrFwd * 100,
                            TenYearFwd = TenYrFwd * 100)
    
    PrepaymentAssumption <- PrepaymentModel(bond.id = bond.id, 
                                                 MortgageRate = MortgageRate,
                                                 TermStructure = CIRTermStructure, 
                                                 PrepaymentAssumption = "MODEL", 
                                                 ModelTune = ModelTune, 
                                                 Burnout = Burnout,
                                                 Severity = 0) 

    
    #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, 
    #Duration, Convexity and CashFlow.
    MortgageCashFlow <- MortgageCashFlow(bond.id = bond.id, 
                                         original.bal = original.bal, 
                                         settlement.date = settlement.date, 
                                         price = price, 
                                         PrepaymentAssumption = PrepaymentAssumption)
    
    MortgageKeyRate <- MtgTermStructure(bond.id = bond.id,
                                       original.bal = original.bal,
                                       Rate.Delta = rate.delta,
                                       TermStructure = CIRTermStructure,
                                       settlement.date = settlement.date,
                                       principal = principal,
                                       price = price,
                                       cashflow = MortgageCashFlow)
    
    # --------------------------------------------------------------------------------------------------------
    # Solve for the zero volatility spread
    # --------------------------------------------------------------------------------------------------------
    
    cashflow.length <- length(MortgageCashFlow@TotalCashFlow)
    SpotSpread <- function(spread = numeric(), 
                           cashflow = vector(), 
                           discount.rates = vector(), 
                           t.period = vector(), 
                           proceeds = numeric()){
      
      Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
      return(proceeds - Present.Value)}
    
    SolveSpotSpread <- uniroot(SpotSpread, 
                               interval = c(-.75, .75), 
                               tol = tolerance, 
                               cashflow = MortgageCashFlow@TotalCashFlow,
                               discount.rates = CIRTermStructure@SpotRate[1:cashflow.length]/yield.basis,
                               t.period = MortgageCashFlow@TimePeriod,
                               #t.period = CIRTermStructure@Period[1:cashflow.length] , 
                               proceeds = proceeds)$root
    
    spot.spread <- SolveSpotSpread
    
   
    new("MortgageOAS",
       OAS = OAS.Spread,
       ZVSpread = spot.spread,
       SpreadToCurve = SpreadtoCurve,
       EffDuration = MortgageKeyRate@EffDuration,
       EffConvexity = MortgageKeyRate@EffConvexity,
       KeyRateTenor = MortgageKeyRate@KeyRateTenor,
       KeyRateDuration = MortgageKeyRate@KeyRateDuration,
       KeyRateConvexity = MortgageKeyRate@KeyRateConvexity,
       PathSpread = OAS.Out[,1],
       PathWAL = OAS.Out[,2],
       PathModDur = OAS.Out[,3],
       PathYTM =OAS.Out[,4],
       PriceDist = OAS.Out[,5])}
