  
  require(boot)
  
  Factors <- read.csv('~/Documents/FabozziModel/DLFactors.csv', 
                      header = TRUE, sep = ',',
                      colClasses = c(
                                    'character', #date
                                    'numeric',  #beta1
                                    'numeric',  #beta2
                                    'numeric',  #beta3
                                    'numeric',  #lambda
                                    'numeric',  #level
                                    'numeric',  #slope
                                    'numeric'   #curve
                                    ))
   # set the number of simulation paths used
   numpaths = 300
   paths = seq(1, numpaths, 1)
  
   # the list of lists of simulation paths used to loop over the data
   # and simulate the paths for each dl coefficient
   dlfactors <- list('level', 'slope', 'curvature')
   
   # create a list of for each set of coefficents
   level <- list()
   slope <- list()
   curvature <- list()
   
   # create a list of mean reversion factors for each factors
   kappa <- list(
     level = .03,
     slope = .03,
     curvature = .03
   )
  
  # calculate the beta deltas
  dldelta <- list(
  level = Factors$beta1[2:184] - Factors$beta1[1:183],
  slope = Factors$beta2[2:184] - Factors$beta2[1:183],
  curvature = Factors$beta2[2:184] - Factors$beta2[1:183]
  )
  
  # calculate the means of the coefficients
  coeffmeans <- list(
  level = mean(Factors$beta1),
  slope = mean(Factors$beta2),
  curvature = mean(Factors$beta3))
  
  # a list of the current coefficent values
  dlcoefficient <- list(
    level = Factors$beta1[184],
    slope = Factors$beta2[184],
    curvature = Factors$beta3[184]
  )
  
  # create a list of the rates needed for OAS calcualton
  ratedata <- list(
    onemonth = numeric(480),
    twoyear = numeric(480),
    tenyear = numeric(480),
    mtgrate = numeric(480)
  )
 
  # Helper functions for arima simulation
  ar_fun <- function(ts) c(ar = coef(arima(ts, order = c(1, 0, 0),
                                         include.mean = TRUE)), ts = ts)

  ar_sim <- function(res, n.sim, ran.args) {
    rg <- function(n, res) sample(res, n, replace = TRUE)
    ts <- ran.args$ts
    model <- ran.args$model
    arima.sim(model = model, n = n.sim,
              rand.gen = rg, res = c(res))}
  
  start <- proc.time()
  # Start the simulation
  for(factor in seq_along(dlfactors)){
  ar_fit <- arima(dldelta[[factor]], order = c(1, 0, 0), include.mean = TRUE)
  ts_res <- residuals(ar_fit)
  ts_res <- ts_res - mean(ts_res)
  ar_model <- list(ar = coef(ar_fit))

  for(path in seq_along(paths)){
  periods = seq(1,480,1)
  newvalue <- numeric(480)
  
  ar_boot <- tsboot(ts_res, ar_fun,
                    R = 1, sim = "model",
                    n.sim = 480, orig.t = FALSE,
                    ran.gen = ar_sim,
                    ran.args = list(ts = dldelta[[factor]], model = ar_model))
  
  for(period in seq_along(periods)){
    if(period==1) {
      newvalue[period] = (.03 *(coeffmeans[[factor]] - dlcoefficient[[factor]])) + 
        (ar_boot$t[period] + dlcoefficient[[factor]])
    } else {newvalue[period] = (.03 * (coeffmeans[[factor]] - newvalue[period-1])) + 
      (ar_boot$t[period] + 
         newvalue[period-1])}
  }
  if(dlfactors[[factor]] == 'level') level[[paste0('path',path)]] <- newvalue
  if(dlfactors[[factor]] == 'slope') slope[[paste0('path',path)]] <- newvalue
  if(dlfactors[[factor]] == 'curvature') curvature[[paste0('path',path)]] <- newvalue
  }
  }
  
  for(period in seq_along(periods)){
    ratedata$onemonth[period] = termstrc::spotrates(method = 'dl', 
                                                    beta = c(level[[1]][period],
                                                             slope[[1]][period],
                                                             curvature[[1]][period]),
                                                    m = 1/12)
  }

  
 