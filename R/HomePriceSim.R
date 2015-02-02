HPISim <- function(shortrate = numeric(),
                   LongTermGrowth = numeric(),
                   T = numeric(),
                   step = numeric(),
                   sigma = numeric(),
                   N = numeric()){
  #short rate is the short term rate and is passed from the CIR model to the HIP Model
  #OER is the owner's equivalent rent
  #T is the horizon of the simulation
  #step is the step of the simulation for MBS step is monthly (1/12)
  
  dt <- step
  nrow <- T/dt
                     
  deltahomeprice <- function(shortrate = numeric(),
                             LongTermGrowth = numeric(),
                             step = numeric(),
                             sigma = numeric()){
  ((shortrate - LongTermGrowth) * dt) + (sigma *  sqrt(abs(homesimulation[i-1,j])) * rnorm(1,0,1))}
  
  homesimulation = array(data = 0, c((nrow + 1), N))
  homesimulation[1,] = 1
  shortrate = ((1 + shortrate) ^(1/12))-1
  LongTermGrowth = ((1 + LongTermGrowth)^(1/12)) -1

  for(j in 1:N){
    for(i in 2:(nrow + 1)){
      homesimulation[i,j] <- homesimulation[i-1, j] + deltahomeprice(shortrate = shortrate, 
                                                                     LongTermGrowth= LongTermGrowth, 
                                                                     step = dt,
                                                                     sigma = sigma)
    }
  }
  colnames(homesimulation) <- c(rep((paste("path", seq(1:N)))))
  return(homesimulation)
}

