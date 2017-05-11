
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
  
  #'@include CashFlowEngine.R MortgageKeyRate.R

  #Function to calcualte the spread over the discount curve
  SpotSpread <- function(
    spread = numeric(),
    cashflow = vector(),
    discount.rates = vector(),
    t.period = vector(),
    proceeds = numeric()){
    Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
    return(proceeds - Present.Value)}


#Define the mean reversion function used to simulate the 3mo, 2-, and 10-
#year interest rates which will define the level, slope and curvature 
#coefficients of the Dibold Li model

MonteCarloMeanReversion <- function(initial.value, mean, sigma, reversion.speed, step){
  drift.1 = log(initial.value) * exp(-reversion.speed * step)
  drift.2 = log(mean) * (1-(exp(-reversion.speed*step)))
  stochastic = sigma * sqrt((1-exp(-2*reversion.speed * step))/(2*reversion.speed)) * rnorm(1, 0, 1)
  simulated.delta = drift.1 + drift.2 + stochastic
  simulated.value = exp(simulated.delta - (.5 * (1-exp(-2*reversion.speed*step)) * ((sigma^2)/(2*reversion.speed))))
  return(simulated.value)}

#Function to simulate the three month, 2-year and 10-year rates
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

  #Function to simulate the three month, 2-year and 10-year rates
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
      initial.value = rates.data[1,'USSW2']/1,
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

  FitMarket <- function(param,
                        rate.to.simulate,
                        rates.data, 
                        num.periods = 480, 
                        num.paths = 300){
  
    invisible(capture.output(term.structure <-
                               TermStructure(rates.data = rates.data, method = 'dl')))
    fwd.curve = ForwardRate(term.structure)[1:num.periods]/1
  
    simulation <- sim.rates(rates.data = rates.data, 
                            num.paths = num.paths,
                            rate.to.simulate = rate.to.simulate,
                            parameters = param)
  
    mkt.fwd.rate.tenor = rates.data[2,rate.to.simulate] * 12                        
    mkt.fwd.rate = fwd.curve[mkt.fwd.rate.tenor]
  
    sum.squared.diff = sum(colMeans(simulation)['Rate'] - mkt.fwd.rate)^2
  
    return(sum.squared.diff)
    }


  CalibrateRates <- function(rates.data,
                             num.paths = 300){
    
    params.3M = optim(par = c(1.5, 0.3, 0.2),
                      fn = fit.to.market,
                      method = "L-BFGS-B",
                      lower = c(0.10, .05, 0.10),
                      upper = c(3.00, .30, 0.50),
                      rates.data = rates.data,
                      rate.to.simulate = 'ED3M',
                      num.paths = num.paths,
                      control = list(maxit = 500,
                                   factr = 1e-08,
                                   pgtol = 1e-6))$par
  
    params.24M = optim(par = c(1.5, 0.3, 0.2),
                       fn = fit.to.market,
                       method = "L-BFGS-B",
                       lower = c(0.20, .05, 0.10),
                       upper = c(3.50, .30, 0.50),
                       rates.data = rates.data,
                       rate.to.simulate = 'USSW2',
                       num.paths = num.paths,
                       control = list(maxit = 5000,
                                      factr = 1e-08,
                                      pgtol = 1e-6))$par
  
    params.120M = optim(par = c(2.23, 0.3, 0.2),
                        fn = fit.to.market,
                        method = "L-BFGS-B",
                        lower = c(2.0, .05, 0.05),
                        upper = c(8.0, .20, 0.30),
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


  OASRatesArray <- function(rates.data,
                            num.paths = 300,
                            num.periods = 480,
                            calibration){
    # Create the curve simulation matrix - the curve parameters needed to 
    # drive the DL model and simulate the yield curve
  
    simulation = sim.curve(rates.data = rates.data,
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
      spot.rate = spr_dl(beta = c(beta.1, beta.2, beta.1), 
                         m = seq(1/12, (num.periods + 120)/12, 1/12),
                         lambda = .731)
      fwd.rate = fwr_dl(beta = c(beta.1, beta.2, beta.1), 
                        m = seq(1/12, num.periods/12, 1/12),
                        lambda = .731)
    
      OAS.array[,period,1] = spot.rate[1:num.periods]
      OAS.array[,period,2] = fwd.rate
      OAS.array[,period,3] = Forward.Rate(SpotRate.Curve = spot.rate, FwdRate.Tenor =  24)[1:num.periods]
      OAS.array[,period,4] = Forward.Rate(SpotRate.Curve = spot.rate, FwdRate.Tenor =  120)[1:num.periods]
    } # end of oas rate array loop
    return(OAS.array)
    }

  # This function is the mortgage OAS function built upon the oas calibration
  # and the oas.array functions.  
  MortgageOAS <- function(bond.id,
                          price,
                          trade.date,
                          settlement.date,
                          OAS.array,
                          num.periods = 480){
    
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
    ModelTune <- ModelTune(Pool)
    Burnount <- BurnOut(Pool)
  
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
  
  
    for(paths in 1:ncol(oas.array)){
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
    
      discount.spreads[paths] <- uniroot(Spot.Spread, 
                                         interval = c(-1, 1), 
                                         tol = .0000000001, 
                                         cashflow = cash.flow.array[,3],
                                         discount.rates = OAS.array[,paths,2]/100, 
                                         t.period = cash.flow.array[,2], 
                                         proceeds = proceeds)$root}
    return(discount.spreads)
    }