# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


#---------------------------------
#Time value of money function
#---------------------------------

TimeValue <- function(interest.rate = numeric(), number.periods = numeric(), frequency = numeric(), type = "character"){
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate <0 | interest.rate > 1)
    stop("No valid  interest.rate specified.")
  
  if(missing(number.periods))
    stop("Need to provide the number of discounting periods")
  if(!is.numeric(number.periods))
    stop(" No numeric discounting period specfified")
  if (number.periods < 1 )
    stop("No valid  number.periods specified.")
  
  if(missing(frequency))
    stop("Need to provide the frequency")
  if(!is.numeric(frequency))
    stop(" No numeric frequency specfified")
  if (frequency < 1 | frequency >12 )
    stop("No valid frequency specified.")
  
  if(! type %in% c("PV", "PVA", "PVAD", "FV", "FVA"))
    stop("Time value function not specfied correctly")
  
  interest.rate = interest.rate/frequency
  
  switch(type,
         PV =  1/(1+interest.rate)^number.periods,
         PVA = ((1-(1/(1+interest.rate)^number.periods))/interest.rate),
         PVAD =   ((1-(1/(1+interest.rate)^number.periods))/interest.rate) * (1+interest.rate),  
         FV =  (1+interest.rate)^number.periods,
         FVA =   (((1 + interest.rate)^(number.periods)) -1)/interest.rate)
  
}