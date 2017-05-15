
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

  # The following script is used to calculate Term Structure
  # metrics for mortgage backed securities.  To create the script
  # the standard procedure is followed set class, set generics,
  # set methods, functions.  This class is a sub class (document superclass)

  #'@include CashFlowEngine.R PrepaymentModel.R MBSDetails.R CurveSpreads.R
  NULL
  
  #'@title mortgage oas class
  #'@family mortgage pass through option adjusted spread
  #'@description mortgage pass through oas class
  #'@slot Cusip the pass through cusip
  #'@slot Issuer the pass through issuer
  #'@slot Coupon the pass through investor coupon
  #'@slot Term the pass though amortization term 
  #'@slot OAS the mortgage option adjusted spread
  #'@slot ZeroVolSpread the zero volatility spread
  #'@slot Spreads the path spread along each simulation
  #'@slot OAD option adjusted duration
  #'@slot OAC option adjusted convexity
  #'@exportClass MortgageOAS
  setClass('MortgageOAS',
           representation(
             Cusip = 'character',
             Issuer = 'character',
             Coupon = 'numeric',
             Term = 'numeric',
             OAS = 'numeric',
             ZeroVolSpread = 'numeric',
             Spreads = 'numeric',
             OAD = 'numeric',
             OAC = 'numeric'
           ))
  
  #'@title generic function to plot OAS
  #'@description standard generic to polt MortgageOAS
  #'@param object MortgageOAS object
  #'@export PlotOAS
  setGeneric('PlotOAS', function(object)
  {standardGeneric('PlotOAS')})
  
  # Note standard generic Cusip is defined in MBSDetails.R
  # Note standard generic Issuer is defined in MBSDetails.R
  # Note standard generic Coupon is defined in MBSDetails.R
  # Note standard generic Term is defined in MBSDetails.R
  # Note standard generic ZeroVolSpread is defined in CurveSpreads.R

  #'@title A standard generic to access the slot OAS
  #'@description Access the slot OAS from object MortgageOAS
  #'@param object MortgageOAS
  #'@export OAS
  setGeneric('OAS', function(object)
    {standardGeneric('OAS')})
  
  #'@title A standard generic to access the slot Spreads
  #'@description Access the slot Spreads from object MortgageOAS
  #'@param object MortgageOAS
  #'@export Spreads
  setGeneric('Spreads', function(object)
    {standardGeneric('Spreads')})
  
  #'@title A standard generic to access the slot OAD
  #'@description Access the slot OAD from object MortgageOAS
  #'@param object MortgageOAS
  #'@export OAD
  setGeneric('OAD', function(object)
    {standardGeneric('OAD')})
  
  #'@title A standard generic to access the slot OAC
  #'@description Access the slot OAC from object MortgageOAS
  #'@param object MortgageOAS
  #'@export OAC
  setGeneric('OAC', function(object)
    {standardGeneric('OAC')})
  
  setMethod('initialize',
            signature('MortgageOAS'),
            function(.Object,
                     Cusip = 'character',
                     Issuer = 'character',
                     Coupon = numeric(),
                     Term = numeric(),
                     OAS = numeric(),
                     ZeroVolSpread = numeric(),
                     Spreads = numeric(),
                     OAD = numeric(),
                     OAC = numeric(),
                     ...){
              callNextMethod(.Object,
                             Cusip = Cusip,
                             Issuer = Issuer,
                             Coupon = Coupon,
                             Term = Term,
                             OAS = OAS,
                             ZeroVolSpread = ZeroVolSpread,
                             Spreads = Spreads,
                             OAD = OAD,
                             OAC = OAC,
                             ...)
            })
  
  #'@title A method to access the slot cusip
  #'@description Access the slot Cusip from object MortgageOAS
  #'@param object MortgageOAS object
  #'@exportMethod Cusip
  setMethod('Cusip', signature('MortgageOAS'),
            function(object){object@Cusip})
  
  #'@title A method to access the slot Issuer
  #'@description Access the slot Issuer from object MortgageOAS
  #'@param object MortgageOAS object
  #'@exportMethod Issuer
  setMethod('Issuer', signature('MortgageOAS'),
            function(object){object@Issuer})
  
  #'@title A Method to access the slot Coupon
  #'@description Access the slot Coupon from object MortgageOAS
  #'@param object MortgageOAS object
  #'@exportMethod Coupon
  setMethod('Coupon', signature('MortgageOAS'),
            function(object){object@Coupon})
  
  #'@title A Method to access the slot Term
  #'@description Access the slot Term from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod Term
  setMethod('Term', signature('MortgageOAS'),
            function(object){object@Term})
  
  #'@title A Method to access the slot OAS
  #'@description Access the slot OAS from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod OAS
  setMethod('OAS', signature('MortgageOAS'),
            function(object){object@OAS})
  
  #'@title A Method to access the slot ZeroVolSpread
  #'@description Access the slot ZeroVolSpread from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod ZeroVolSpread
  setMethod('ZeroVolSpread', signature('MortgageOAS'),
            function(object){object@ZeroVolSpread})
  
  #'@title A Method to access the slot Spreads
  #'@description Access the slot Spreads from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod Spreads
  setMethod('Spreads', signature('MortgageOAS'),
            function(object){object@Spreads})
  
  #'@title A Method to access the slot OAD
  #'@description Access the slot OAD from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod OAD
  setMethod('OAD', signature('MortgageOAS'),
            function(object){object@OAD})
  
  #'@title A Method to access the slot OAC
  #'@description Access the slot OAC from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod OAC
  setMethod('OAC', signature('MortgageOAS'),
            function(object){object@OAC})
  
  #'@title Method to plot MortgageOAS spread distribution
  #'@description A method to plot MortgageOAS spread distribution
  #'@param object MortgageOAS object
  #'@importFrom stats ecdf
  #'@importFrom grDevices rgb
  #'@importFrom graphics axis grid hist lines mtext par plot
  #'@exportMethod PlotOAS
  setMethod('PlotOAS', signature('MortgageOAS'),
            function(object){
              par(mar = c(5,5,2,5), lwd = 2)
              h <- hist(object@Spreads,
                        main = paste(object@Issuer, 
                                     format(round(object@Coupon,2), nsmall =2), 
                                     object@Term,'-year OAS Distribution'),
                        sub = paste('Paths', 
                                    length(object@Spreads), 
                                    'OAS =', format(round(object@OAS,2), nsmall = 2), 
                                    sep = " "),
                        ylab = 'Frequency', 
                        xlab = 'Spread', 
                        col = 'blue', 
                        border = 'grey')
              par(new = T, lwd = 1)
              ec <- ecdf(object@Spreads)
              plot(x = h$mids, y=ec(h$mids)*max(h$counts), 
                   col = rgb(0,0,0,alpha=0), 
                   axes=F, 
                   xlab=NA, 
                   ylab=NA)
              lines(x = h$mids, 
                    y=ec(h$mids)*max(h$counts), 
                    col ='orange', 
                    lwd = 3)
              axis(4, 
                   at=seq(from = 0, to = max(h$counts), 
                          length.out = 11), 
                   labels=seq(0, 1, 0.1), 
                   col = 'black', 
                   col.axis = 'black')
              mtext(side = 4, 
                    line = 3, 
                    'Cumulative Density', 
                    col = 'black')
              grid(col = 'grey', lty = 'dotted')
            })

  #Define the mean reversion function used to simulate the 3mo, 2-, and 10-
  #year interest rates which will define the level, slope and curvature
  #coefficients of the Dibold Li model
  
  #'@title MonteCarloMeanReversion
  #'@family mortgage pass through option adjusted spread
  #'@description Monte Carlo Simulation
  #'@param initial.value the initial value
  #'@param mean the mean of the asset to simulate
  #'@param sigma the volatility of the asset to simulate
  #'@param reversion.speed the speed of the mean reversion
  #'@param step the time step defaults to 1/12
  #'@importFrom stats rnorm
  #'@export MonteCarloMeanReversion
  MonteCarloMeanReversion <- function(initial.value, mean, sigma, reversion.speed, step = 1/12){
    drift.1 = log(initial.value) * exp(-reversion.speed * step)
    drift.2 = log(mean) * (1-(exp(-reversion.speed*step)))
    stochastic = sigma * sqrt((1-exp(-2*reversion.speed * step))/(2*reversion.speed)) * rnorm(1, 0, 1)
    simulated.delta = drift.1 + drift.2 + stochastic
    simulated.value = exp(simulated.delta - (.5 * (1-exp(-2*reversion.speed*step)) * ((sigma^2)/(2*reversion.speed))))
    return(simulated.value)}
  

  
 
  #'@title SimRates
  #'@family mortgage pass through option adjusted spread
  #'@description A function to simulate interest rate paths.  The function is called
  #'by FitMarket and used to fit the forward rate curve
  #'@param rates.data A character refrencing a rates.data object
  #'@param num.paths The number of interest rate paths to simulate defaults to 300
  #'@param seed The random seed defaults to 42
  #'@param rate.to.simulate The rate to simulate
  #'@param parameters A vector parameters mean, standard deviation, and the rate
  #'of mean reversion of the rate simulated
  #'@export SimRates
  SimRates <- function(rates.data,
                       num.paths = 300,
                       seed = 42,
                       rate.to.simulate,
                       parameters){
  # note error trap the rate input using rates.data colnames
  set.seed(seed)
    
  
  #set up the monte carlo simulation of the rates for the dl three factor OAS
  #model dimension the array which will be used to hold the simulated interest
  #rates.  These rates will be passed to dl as level slope and curvature
  rates.names <- c("Path",
                   "Rate")
  
  rates.array = array(data = NA, dim = c(num.paths, 2),
                      dimnames = list(seq(c(1:num.paths)), rates.names))
  
  for(period in 1:num.paths){
    rates.array[period, 'Path'] = period
    
    if(period ==1) {rates.array[period, 'Rate'] = MonteCarloMeanReversion(
      initial.value = rates.data[1,rate.to.simulate],
      mean = parameters[1],
      sigma = parameters[2],
      reversion.speed = parameters[3],
      step = 1/12)
    } else {rates.array[period, 'Rate'] = MonteCarloMeanReversion(
      initial.value = rates.array[period - 1, 'Rate'],
      mean = parameters[1],
      sigma = parameters[2],
      reversion.speed = parameters[3],
      step = 1/12)}
    }
  return(rates.array)
  }

  #'@title SimCurve Yield Curve Simulation
  #'@family mortgage pass through option adjusted spread
  #'@description Simulation of the 3-mo, 2-year, and 10-year forward.
  #'The function is used by the OASRatesArray function to create the rate array
  #'@param rates.data A character referencing a rates.data object
  #'@param num.paths The number of simulation paths
  #'@param seed The value of the random seed defaults to 42
  #'@param three.month A vector of the mean, standard deviation, and mean 
  #'reversion rate of the three month
  #'@param two.year A vector of the mean, standard deviation and mean reversion
  #'rate of the two year
  #'@param ten.year A vector of the mean, standard deviation and mean reversion
  #'rate of the ten year
  #'@export SimCurve
  SimCurve <- function(rates.data,
                       num.paths = 300,
                       seed = 42,
                       three.month,
                       two.year,
                       ten.year){
  set.seed(seed)
  #set up the monte carlo simulation of the rates for the dl three factor OAS
  #model dimension the array which will be used to hold the simulated interest
  #rates.  These rates will be passed to dl as level slope and curvature
  rates.names <- c("Path",
                   "ThreeMonth",
                   "TwoYear",
                   "TenYear")
  
  rates.array = array(data = NA, dim = c(num.paths, 4),
                      dimnames = list(seq(c(1:num.paths)), rates.names))
  
  for(period in 1:num.paths){
    rates.array[period, 'Path'] = period
    
    if(period ==1) {rates.array[period, 'ThreeMonth'] = MonteCarloMeanReversion(
      initial.value = rates.data[1,'ED3M'],
      mean = three.month[1],
      sigma = three.month[2],
      reversion.speed = three.month[3],
      step = 1/12)
    } else {rates.array[period, 'ThreeMonth'] = MonteCarloMeanReversion(
      initial.value = rates.array[period - 1, 'ThreeMonth'],
      mean = three.month[1],
      sigma = three.month[2],
      reversion.speed = three.month[3],
      step = 1/12)}
    
    if(period ==1) {rates.array[period, 'TwoYear'] = MonteCarloMeanReversion(
      initial.value = rates.data[1,'USSW2'],
      mean = two.year[1],
      sigma = two.year[2],
      reversion.speed = two.year[3],
      step = 1/12)
    } else {rates.array[period, 'TwoYear'] = MonteCarloMeanReversion(
      initial.value = rates.array[period - 1, 'TwoYear'],
      mean = two.year[1],
      sigma = two.year[2],
      reversion.speed = two.year[3],
      step = 1/12)}
    
    if(period ==1) {rates.array[period, 'TenYear'] = MonteCarloMeanReversion(
      initial.value = rates.data[1,'USSW10'],
      mean = ten.year[1],
      sigma = ten.year[2],
      reversion.speed = ten.year[3],
      step = 1/12)
    } else {rates.array[period, 'TenYear'] = MonteCarloMeanReversion(
      initial.value = rates.array[period - 1, 'TenYear'],
      mean = ten.year[1],
      sigma = ten.year[2],
      reversion.speed = ten.year[3],
      step = 1/12)}
  }
  return(rates.array)
  }

  #'@title FitMarket
  #'@family mortgage pass through option adjusted spread 
  #'@description Function passed to CalibrateRates to fit forward rate curve.
  #'This function is used by CalibrateRates optimization.
  #'@param param the parameters optimized by CalibrateRates
  #'@param rate.to.simulate rate to simulate
  #'@param rates.data A character referencing a rates.data object
  #'@param num.periods The number of mortgage payments defaults to 480
  #'@param num.paths The number of interest rate paths to simulate defaults to 300
  #'@export FitMarket
  FitMarket <- function(param,
                        rate.to.simulate,
                        rates.data,
                        num.periods = 480,
                        num.paths = 300){
  
  invisible(capture.output(term.structure <-
                             TermStructure(rates.data = rates.data, method = 'dl')))
  fwd.curve = ForwardRate(term.structure)[1:num.periods]
  
  simulation <- SimRates(rates.data = rates.data,
                         num.paths = num.paths,
                         rate.to.simulate = rate.to.simulate,
                         parameters = param)
  
  mkt.fwd.rate.tenor = rates.data[2,rate.to.simulate] * 12
  mkt.fwd.rate = fwd.curve[mkt.fwd.rate.tenor]
  
  sum.squared.diff = sum(colMeans(simulation)['Rate'] - mkt.fwd.rate)^2
  
  return(sum.squared.diff)
  }

  #'@title CalibrateRates
  #'@family mortgage pass through option adjusted spread
  #'@description optimization to fit 3-mo, 2-year, and 10-year rates to the
  #'forward rate curve
  #'@param rates.data A character referencing a rates.data object
  #'@param num.paths the number of simulations to fit to the market 
  #'defaults to 5,000
  #'@importFrom stats optim
  #'@export CalibrateRates
  CalibrateRates <- function(rates.data,
                             num.paths = 5000){

  params.3M = optim(par = c(1.5, 0.3, 0.2),
                    fn = FitMarket,
                    method = "L-BFGS-B",
                    lower = c(0.50, 0.10, 0.20),
                    upper = c(3.00, 0.30, 0.30),
                    rates.data = rates.data,
                    rate.to.simulate = 'ED3M',
                    num.paths = num.paths,
                    control = list(maxit = 500,
                                   factr = 1e-08,
                                   pgtol = 1e-6))$par
  
  params.24M = optim(par = c(1.5, 0.3, 0.2),
                     fn = FitMarket,
                     method = "L-BFGS-B",
                     lower = c(1.50, .10, 0.20),
                     upper = c(5.50, .30, 0.30),
                     rates.data = rates.data,
                     rate.to.simulate = 'USSW2',
                     num.paths = num.paths,
                     control = list(maxit = 5000,
                                    factr = 1e-08,
                                    pgtol = 1e-6))$par
  
  params.120M = optim(par = c(2.23, 0.3, 0.2),
                      fn = FitMarket,
                      method = "L-BFGS-B",
                      lower = c(3.5, .10, 0.20),
                      upper = c(8.0, .30, 0.40),
                      rates.data = rates.data,
                      rate.to.simulate = 'USSW10',
                      num.paths = num.paths,
                      control = list(maxit = 5000,
                                     factr = 1e-8,
                                     pgtol = 1e-6))$par
  
  parameters = rbind(params.3M, params.24M, params.120M)
  colnames(parameters) = c('mean', 'std.dev', 'mean.reversion')
  return(parameters)
  }

  #'@title Option Adjusted Spread Rates Array
  #'@family mortgage pass through option adjusted spread
  #'@description Simulated rates paths for option adjusted spread analysis
  #'@param rates.data A character referencing a rates.data object
  #'@param num.paths The number of simulated paths defaults to 300
  #'@param num.periods The number of mortgage payment periods defaults to 480
  #'do not change unless you know what you are doing
  #'@param calibration A character referencing the rate calibration object
  #'@importFrom termstrc spr_dl
  #'@importFrom termstrc fwr_dl
  #'@export OASRatesArray
  OASRatesArray <- function(rates.data,
                            num.paths = 300,
                            num.periods = 480,
                            calibration){
  # Create the curve simulation matrix - the curve parameters needed to
  # drive the DL model and simulate the yield curve
  
  simulation = SimCurve(rates.data = rates.data,
                         num.paths = num.paths,
                         three.month = unname(calibration['params.3M',]),
                         two.year = unname(calibration['params.24M',]),
                         ten.year = unname(calibration['params.120M',]))
  
  # The first element is the number of periods (rows), and the second eleemnt
  # is the paths (cloumns), the third element is the matrix type
  
  # dimension array for oas analysis first the spot rate curve is calculated
  # the first array is the spot rate, the second is the 1mo fwd rate, the
  # third is the two-year, the fourth is the ten-year
  # Populate the OAS array with the discount rates, spot rates, and forward
  # rates that are needed to discount cashflows and run the prepayment model
  OAS.array <- array(data = 0, dim = c(num.periods, num.paths, 4))
  
  # populate the array for OAS analysis this array will be passed to OAS function
  # the idea is to separate the OAS rate array from the OAS calculation and pass
  # the array to the calculation.
  
  for(period in 1:num.paths){
    beta.1 = simulation[period,'TenYear']
    beta.2 = simulation[period,'ThreeMonth'] - simulation[period,'TenYear']
    beta.3 = 2 * (simulation[period,'TwoYear'] -(simulation[period,'ThreeMonth'] + simulation[period,'TenYear']))
    spot.rate = termstrc::spr_dl(beta = c(beta.1, beta.2, beta.3),
                       m = seq(1/12, (num.periods + 120)/12, 1/12),
                       lambda = .731)
    fwd.rate = termstrc::fwr_dl(beta = c(beta.1, beta.2, beta.3),
                      m = seq(1/12, num.periods/12, 1/12),
                      lambda = .731)
    
    OAS.array[,period,1] = spot.rate[1:num.periods]
    OAS.array[,period,2] = fwd.rate
    OAS.array[,period,3] = Forward.Rate(SpotRate.Curve = spot.rate, FwdRate.Tenor =  24)[1:num.periods]
    OAS.array[,period,4] = Forward.Rate(SpotRate.Curve = spot.rate, FwdRate.Tenor =  120)[1:num.periods]
  } # end of oas rate array loop
  return(OAS.array)
  }

  #'@title Mortgage PassThrough Option Adjusted Spread
  #'@family mortgage pass though option adjusted spread
  #'@description A function to compute mortgage passthrough option adjusted spread
  #'@param bond.id A character the bond.id
  #'@param price A character the price of the bond given in decimal of 32nds
  #'@param trade.date A character the trade date mm-dd-YYYY
  #'@param settlement.date A character the settlement date mm-dd-YYYY
  #'@param OAS.array An array of simulated rate vectors from the function OASArray
  #'@param num.periods the number of payment periods defaults to 480 do not change 
  #'this unless you know what you are doing
  #'@importFrom stats optim
  #'@export MortgageOAS
  MortgageOAS <- function(bond.id,
                          price,
                          trade.date,
                          settlement.date,
                          OAS.array,
                          num.periods = 480){
  
  #Function to calcualte the spread over the discount curve
  SpotSpread <- function(
    spread = numeric(),
    cashflow = vector(),
    discount.rates = vector(),
    t.period = vector(),
    proceeds = numeric()){
    Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
    return(proceeds - Present.Value)}
  
  # This section begins the OAS analysis.  the code above is market fit and
  # should be run before OAS and assigned to a market rates environment
  
  cash.flow.array = array(data = 0, dim = c(num.periods, 3))
  # use basis conversion for period in OAS as CRT deals pay on actual/360
  cash.flow.array[,1] = seq(1:num.periods)
  cash.flow.array[,2] = cash.flow.array[,1]/12
  
  basis.conversion.factor = BondBasisConversion(
    issue.date = as.Date(IssueDate(bond.id), format = '%m-%d-%Y'),
    start.date = as.Date(DatedDate(bond.id), format = '%m-%d-%Y'),
    end.date = as.Date(Maturity(bond.id), format = '%m-%d-%Y'),
    settlement.date = as.Date(settlement.date, format = '%m-%d-%Y'),
    lastpmt.date = as.Date(LastPmtDate(bond.id), format = '%m-%d-%Y') + PaymentDelay(bond.id),
    nextpmt.date = as.Date(NextPmtDate(bond.id), format = '%m-%d-%Y') + PaymentDelay(bond.id),
    type = BondBasis(bond.id))
  
  principal = CurrentBal(bond.id)
  price = PriceTypes(price)
  accrued = Coupon(bond.id)/100 * principal * basis.conversion.factor
  proceeds = (PriceBasis(price) * principal) + accrued
  discount.spreads = numeric(ncol(OAS.array))
  
  MtgRate <- MtgRate()
  ModelTune <- ModelTune(bond.id)
  Burnout <- BurnOut(bond.id)
  
  OAS.Term.Structure <- new("TermStructure",
                            TradeDate = as.character(trade.date),
                            Period = numeric(),
                            Date = "character",
                            SpotRate = numeric(),
                            ForwardRate = numeric(),
                            TwoYearFwd = numeric(),
                            TenYearFwd = numeric())
  
  Period(OAS.Term.Structure) <- as.numeric(seq(1,num.periods,1))
  ForwardDate(OAS.Term.Structure) <-
    as.character(as.Date(TradeDate(OAS.Term.Structure), format ="%m-%d-%Y") %m+% months(1:num.periods))
  
  
  for(paths in 1:ncol(OAS.array)){
    SpotRate(OAS.Term.Structure) <- OAS.array[,paths,1]
    ForwardRate(OAS.Term.Structure) <- OAS.array[,paths,2]
    TwoYearForward(OAS.Term.Structure) <- OAS.array[,paths,3]
    TenYearForward(OAS.Term.Structure) <- OAS.array[,paths,4]

    prepayment <- PrepaymentModel(bond.id = bond.id,
                                  TermStructure = OAS.Term.Structure,
                                  MortgageRate = MtgRate,
                                  ModelTune = ModelTune,
                                  Burnout = Burnout,
                                  PrepaymentAssumption = "MODEL")
    
    CashFlow <- CashFlowEngine(
      bond.id = bond.id,
      settlement.date = trade.date,
      principal = principal,
      PrepaymentAssumption = prepayment)[,'Investor CashFlow']
    
    cash.flow.array[1:length(CashFlow), 3] = CashFlow
    
    discount.spreads[paths] <- tryCatch(uniroot(SpotSpread,
                                       interval = c(-.05, .05),
                                       tol = .00000001,
                                       extendInt = c('yes'),
                                       cashflow = cash.flow.array[,3],
                                       discount.rates = OAS.array[,paths,2]/100,
                                       t.period = cash.flow.array[,2],
                                       proceeds = proceeds)$root,
                                       error = function(e) 0)
    }
  
  # Calculate the zero volatility spread
  SpotRate(OAS.Term.Structure) = rowMeans(OAS.array[,,1])
  ForwardRate(OAS.Term.Structure) <- rowMeans(OAS.array[,,2])
  TwoYearForward(OAS.Term.Structure) <- rowMeans(OAS.array[,,3])
  TenYearForward(OAS.Term.Structure) <- rowMeans(OAS.array[,,4])
  
  
  prepayment <- PrepaymentModel(bond.id = bond.id,
                                TermStructure = OAS.Term.Structure,
                                MortgageRate = MtgRate,
                                ModelTune = ModelTune,
                                Burnout = Burnout,
                                PrepaymentAssumption = "MODEL") 
  CashFlow <- CashFlowEngine(
    bond.id = bond.id,
    settlement.date = trade.date,
    principal = principal,
    PrepaymentAssumption = prepayment)[,'Investor CashFlow']
  
  cash.flow.array[1:length(CashFlow), 3] = CashFlow
  
  Zero.Vol.Spread = tryCatch(uniroot(SpotSpread,
                            interval = c(-.05, .05),
                            tol = .0000001,
                            extendInt = c('yes'),
                            cashflow = cash.flow.array[,3],
                            discount.rates = SpotRate(OAS.Term.Structure)/100,
                            t.period = cash.flow.array[,2],
                            proceeds = proceeds)$root,
                            error = function(e) 0)
  
  new('MortgageOAS',
      Cusip = Cusip(bond.id),
      Issuer = Issuer(bond.id),
      Coupon = Coupon(bond.id),
      Term = AmortizationTerm(bond.id),
      OAS = mean(discount.spreads) * yield.basis,
      ZeroVolSpread = Zero.Vol.Spread * yield.basis,
      Spreads = discount.spreads * yield.basis,
      OAD = 999,
      OAC = 999
      )
  }