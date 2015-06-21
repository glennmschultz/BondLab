  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 = File License
  # Copyright (C) 2014  Glenn M Schultz, CFA
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


  #-----------------------------------
  # Mortgage OAS Function
  #___________________________________
  #' The REMIC OAS engine. A constructor function of the Mortgage OAS object
  #' 
  #' REMIC.OAS calculates the option adjusted spread of a REMIC
  #' and creates the MortgageOAS ibject
  #' @param bond.id A character string the bond.id or cusip
  #' @param trade.date A character string the trade date
  #' @param settlement.date A character string the settlement date
  #' @param tranche.price A numeric value the tranche price
  #' @param collateral.price A numeric value the collateral price
  #' @param sigma A numeric value the volatility (not annualized)
  #' @param paths A numeric value the number of paths
  #' @examples REMIC.OAS(bond.id = "BondLabSEQ1", trade.date = "01-10-2013",
  #' settlement.date = "01-13-2013", tranche.price = 100, collateral.price = 100,
  #' sigma = 0.015,paths = 200)
  #' @export
  REMIC.OAS <- function(bond.id = "character", 
                      trade.date = "character", 
                      settlement.date = "character", 
                      #original.bal = numeric(),
                      tranche.price = numeric(),
                      collateral.price = numeric(),
                      #short.rate = numeric(), 
                      sigma = numeric(), 
                      paths = numeric()){
  
  #Open connection to the tranche
  REMIC.Tranche <- MBS(MBS.id = bond.id)
  
  #Open connection to the REMIC Deal
  REMIC.Deal <- REMICDeal(remic.deal = REMIC.Tranche@DealName)
  
  #Open connection to the prepayment model tuning library
  #This has to made to work with multiple collateral groups
  
  Collateral <- MBS(MBS.id = as.character(REMIC.Deal@Group[[1]]@Cusip))
  ModelTune <- ModelTune(Collateral)
  
  # Open connection to the Mortgage Model function
  MortgageRate <- MtgRate()
  
  #Call the desired curve from rates data folder
  # ---- connect to rates data folder
  rates.data <- Rates(trade.date = trade.date)
  
  #-- Note in REMIC data TrancheLastPmtDate is the tranche legal final payment date
  #-- The last payment date is found in the REMIC Deal FactorData List
    
  lastpmt.date <- REMIC.Deal@FactorData[[as.numeric(REMIC.Tranche@TrancheNumber)]]@PaymentDate[length(REMIC.Deal@FactorData[[as.numeric(REMIC.Tranche@TrancheNumber)]]@PaymentDate)]
  
  remic.factor <- REMIC.Deal@FactorData[[as.numeric(REMIC.Tranche@TrancheNumber)]]@Factor[length(REMIC.Deal@FactorData[[as.numeric(REMIC.Tranche@TrancheNumber)]]@Factor)]
  
  tranche.origbal <- as.numeric(REMIC.Deal@Tranches[[as.numeric(REMIC.Tranche@TrancheNumber)]]@TrancheOrigBal)
    
  issue.date = as.Date(REMIC.Tranche@TrancheDatedDate, "%m-%d-%Y")
  start.date = as.Date(REMIC.Tranche@TrancheFirstPmtDate, "%m-%d-%Y")
  end.date = as.Date(REMIC.Tranche@TrancheLastPmtDate, "%m-%d-%Y")
  lastpmt.date = as.Date(lastpmt.date, "%m-%d-%Y")
  nextpmt.date = as.Date(REMIC.Tranche@TrancheNextPmtDate, "%m-%d-%Y")
  coupon = REMIC.Tranche@TrancheCoupon
  frequency = REMIC.Tranche@PrinPmtFrequency
  delay = REMIC.Tranche@Delay
  factor = as.numeric(remic.factor)
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  
  short.rate <- as.numeric(rates.data[1,2])/100
  
  #The spot spread function is used to solve for the spread to the spot curve to normalize discounting
  #This function is encapasulated in term structure
  
  Spot.Spread <- function(spread = numeric(), 
                          cashflow = vector(), 
                          discount.rates = vector(), 
                          t.period = vector(), 
                          proceeds = numeric()){
    
                Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
                return(proceeds - Present.Value)
  }
  
  #First, calibrate the interest rate model to market swap rates and prices
  #Set trade date and call the CalibrateCIR Model
  #trade.date = as.Date(trade.date, "%m-%d-%Y")
  
  Market.Fit <- CalibrateCIR(trade.date = trade.date, sigma = sigma)
  kappa  = Market.Fit$p1
  lambda = Market.Fit$p2
  theta  = Market.Fit$p3
  
  #Calculate the number of cashflows that will be paid from settlement date 
  #to the last pmt date (used end date as next pmdt date for this)
  ncashflows = BondBasisConversion(issue.date = issue.date, 
                                   start.date = start.date, 
                                   end.date = end.date, 
                                   settlement.date = settlement.date,
                                   lastpmt.date = lastpmt.date, 
                                   nextpmt.date = end.date) 
  
  #Build a vector of dates for the payment schedule
  #first get the pmtdate interval
  pmtdate.interval = months.in.year/frequency
  
  #Compute the payment dates 
  pmtdate = as.Date(c(if(settlement.date == issue.date) 
                      {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
                      else 
                      {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y") + delay
  
  
  #Build the time period vector (n) for discounting the cashflows nextpmt date 
  #is vector of payment dates to n for each period
  time.period = BondBasisConversion(issue.date = issue.date, 
                                    start.date = start.date, 
                                    end.date = end.date, 
                                    settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, 
                                    nextpmt.date = pmtdate)
  
  #step4 Count the number of cashflows 
  #num.periods is the total number of cashflows to be received
  #num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  num.period = seq(1:num.periods)

 
  #==== Compute Option Adjusted Spread ==========================================
  #For simulation pass T = mortgage term if the number of paths = 1 then volatility = 0 
  Simulation <- CIRSim(shortrate = short.rate, 
                       kappa = kappa, 
                       theta = theta, 
                       T = ((num.periods-1) / months.in.year), 
                       step = (1/months.in.year), 
                       sigma = sigma, 
                       N = paths)

  #number of rows in the simulation will size the arrays
  num.sim <- nrow(Simulation)

  #Dim arrays for the calculation
  cube.names <- c("Period", "Date", "Time", "SpotRate", "DiscRate", "TwoYear", "TenYear")    
  sim.cube <- array(data = NA, c(num.sim, 7), dimnames = list(seq(c(1:num.sim)),cube.names))
  
  #Populate the simulation cube  
  sim.cube[,1] <- num.period
  sim.cube[,2] <- pmtdate
  sim.cube[,3] <- time.period
  
  #Dimension the arrays that will be needed
  oas.names <- c("OAS", "WAL", "ModDur", "YTM", "Price")
  #OAS out holds OAS solutions to individual trajectory calcualtions solving for the spread to price
  OAS.Out <- array(data = NA, c(paths,5), dimnames = list(seq(c(1:paths)),oas.names))
  
  OAS.CashFlow <- array(data = NA, c(num.sim,paths))
  OAS.DiscMatrix <- array(data = NA, c(num.sim, paths))
  
  for(j in 1:paths){
    
    #calculate spot rate for discounting  
    sim.cube[,4] <- cumprod(1 + Simulation[,j])
    
    #sim.cube 5 is the discount rate to value cash flows
    sim.cube[,5] <- (((sim.cube[,4] ^ (1/ sim.cube[,3]))^(1/months.in.year))-1)
    
    sim.cube[,6] <- as.vector(CIRBondPrice(shortrate = as.numeric(Simulation[, j]), 
                                           kappa = kappa, 
                                           lambda = lambda, 
                                           theta = theta, sigma = sigma, 
                                           T = 2, step = 0, 
                                           result = "y") * 100)
    
    sim.cube[,7] <- as.vector(CIRBondPrice(shortrate = Simulation[, j], 
                                           kappa = kappa, 
                                           lambda = lambda, 
                                           theta = theta, 
                                           sigma = sigma, 
                                           T = 10, 
                                           step = 0, 
                                           result = "y") * 100)
    
    #Initialize OAS Term Structure object.  This object is passed to prepayment assumption
    #Allows the prepayment model to work in the Option Adjusted Spread function replacing Term Structure
    #When sigma is zero the simulated spot rates are compounded forward rates and the two and ten year
    #rates are calcualted from the calculated spot rate rate curve
    
    #if (TermStructure != "TRUE")
      OAS.Term.Structure <- new("TermStructure",
                                tradedate = as.character(trade.date),
                                period = as.numeric(sim.cube[,3]),
                                date = unname(as.character(as.Date(sim.cube[,2], origin = "1970-01-01"))),
                                spotrate = as.numeric(sim.cube[,5]),
                                forwardrate = as.numeric(Simulation[,j]) * 100,
                                TwoYearFwd = as.numeric(sim.cube[,6]),
                                TenYearFwd = as.numeric(sim.cube[,7]))
    
    #else
    #  OAS.Term.Structure <- new("TermStructure",
    #                            tradedate = as.character(as.Date(trade.date, "%m-%d-%Y")),
    #                            period = as.numeric(sim.cube[,1]),
    #                            date = unname(as.character(as.Date(sim.cube[,2], origin = "1970-01-01"))),
    #                            spotrate = as.numeric(sim.cube[,5]) * 100,
    #                            forwardrate = as.numeric(Simulation[,j] * 100),
    #                            TwoYearFwd = as.numeric(sim.cube[,6]),
    #                            TenYearFwd = as.numeric(sim.cube[,7]))
     
    MtgCashFlow <- REMICCashFlow(bond.id = bond.id, 
                    trade.date = trade.date,
                    settlement.date = settlement.date,
                    collateral.price = collateral.price,
                    tranche.price = tranche.price,
                    PrepaymentAssumption = "MODEL",
                    #...,
                    #begin.cpr = begin.cpr,
                    #end.cpr = end.cpr,
                    #seasoning.period = seasoning.period,
                    #CPR = CPR,
                    KeyRateTermStructure = OAS.Term.Structure)
        
   
    OAS.CashFlow[,j] <- as.vector(MtgCashFlow@TotalCashFlow)
    OAS.DiscMatrix [,j] <- as.vector(sim.cube[,5])
    
    #Calculate proceeds to use in OAS solve
    proceeds <- as.numeric((MtgCashFlow@PrincipalProceeds + MtgCashFlow@Accrued))
    #tranche.currbal <- tranche.origbal * factor + (MtgCashFlow@Accrued/(tranche.origbal * factor))
    tranche.currbal <- tranche.origbal * factor + MtgCashFlow@Accrued

    #Solve for spread to spot curve to equal price 
    OAS.Out[j,1] <- uniroot(Spot.Spread, interval = c(-.75, .75), 
                            tol = .0000000001, 
                            cashflow = MtgCashFlow@TotalCashFlow,
                            discount.rates = OAS.Term.Structure@spotrate, 
                            t.period = OAS.Term.Structure@period, 
                            proceeds)$root
    
    OAS.Out[j,2] <- MtgCashFlow@WAL
    OAS.Out[j,3] <- MtgCashFlow@ModDuration
    OAS.Out[j,4] <- MtgCashFlow@YieldToMaturity
    
  } # end of the OAS j loop
  
  #Period <<- data.frame(OAS.Term.Structure@period)
  #CF <<- data.frame(OAS.CashFlow)
  #Disc <<- data.frame(OAS.DiscMatrix)
  
  # Calculate OAS spread find the spread such that the average proceeds is equal to proceeds
  OAS <- function(spread = numeric(), 
                  tranche.principal = "character",
                  tranche.currbal = numeric(),
                  tranche.price = numeric(),
                  DiscountMatrix = matrix(), 
                  CashFlowMatrix = matrix(), 
                  period = vector(), 
                  proceeds = numeric()) {
    
    OAS.Proceeds <- data.frame(((1/((1 + DiscountMatrix[,] + spread) ^ period)) * CashFlowMatrix[,])) 
    
    OAS.Price <- if(isTRUE(as.character(tranche.principal) %in% "NTL" | as.character(tranche.principal) %in% "PO")) 
    {colSums(OAS.Proceeds/tranche.currbal) * 100} else {colSums(OAS.Proceeds/proceeds) * 100}

    
    {return(mean(OAS.Price) - tranche.price)} 
  }
  
  OAS.Spread <- uniroot(OAS, 
                        interval = c(-.75,.75), 
                        tol = .000000001,
                        tranche.principal = as.character(REMIC.Tranche@TranchePrincipalDesc),
                        tranche.currbal = tranche.currbal,
                        tranche.price = tranche.price,
                        DiscountMatrix = OAS.DiscMatrix, 
                        CashFlowMatrix = OAS.CashFlow,
                        period = OAS.Term.Structure@period, 
                        proceeds = proceeds)$root
  
  #Calculate OAS to price for price distribution
  OAS.Price <- function(spread = numeric(), 
                        DiscountMatrix = matrix(), 
                        CashFlowMatrix = matrix(), 
                        period = vector(), 
                        proceeds = numeric(), 
                        paths = numeric()) {
    
    OAS.Proceeds <- data.frame(((1/((1 + OAS.DiscMatrix[,] + spread)^ period)) * OAS.CashFlow[,]))
    
    OAS.Proceeds <- (colSums(OAS.Proceeds)/tranche.currbal) * 100
        
    return(OAS.Proceeds)}
  
  Price.Dist <- OAS.Price(OAS.Spread, 
                          DiscountMatrix = OAS.DiscMatrix, 
                          CashFlowMatrix = OAS.CashFlow,
                          period = OAS.Term.Structure@period, proceeds = proceeds, paths = paths)
  
  OAS.Out[,5] <- Price.Dist 
  
  # Calculate static cash flow spread to the curve at zero volatility
  # Using the prepayment model this will always match the ZV spread indiciating the pricing benchmark is exact
  # In reality the spread to the curve will be based on the pricing speed used.
  # This is a good check but in reality the spread to the curve must be calculated in the PassThrough OAS and passed to 
  # ZeroVolatility Object
  #if (paths != 1) {                                   
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ 
                                as.numeric(rates.data[2,2:12]), data = data.frame(rates.data))  
    
    SpreadtoCurve = ((MtgCashFlow@YieldToMaturity  * 100) - 
                       predict(InterpolateCurve, MtgCashFlow@WAL ))/100

    new("MortgageOAS",
       OAS = OAS.Spread,
       ZVSpread = mean(OAS.Out[,1]),
       SpreadToCurve = SpreadtoCurve,
       PathSpread = OAS.Out[,1],
       PathWAL = OAS.Out[,2],
       PathModDur = OAS.Out[,3],
       PathYTM = OAS.Out[,4],
       PriceDist = OAS.Out[,5]
  )

  
  }
  
  setGeneric("REMIC.OAS", function(bond.id = "character", 
                                   trade.date = "character", 
                                   settlement.date = "character", 
                                   #original.bal = numeric(),
                                   tranche.price = numeric(),
                                   collateral.price = numeric(),
                                   #short.rate = numeric(), 
                                   sigma = numeric(), 
                                   paths = numeric())
    {standardGeneric("REMIC.OAS")})