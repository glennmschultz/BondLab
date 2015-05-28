#Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


#---------------------------------
#Time value of money function
#---------------------------------
#' A function to compute the time value of money
#'                  
#' A standard generic function used to calculate time value of money
#' @param interest.rate The interest rate used to determine the discount factor in decimal
#' form (i.e. 4.0\% is input as 0.04)
#' @param number.periods The number of discount periods.  For example, 
#' in the case of a mortgage whose cash flow occurs monthly over 30 years the number of 
#' periods is 360.
#' @param frequency The frequency of interest payments.  For example the frequency of a
#' mortgage whose payments occur monthly is 12
#' @param type The type of calculation PV = present value, 
#' PVA = present value of annuity, PVAD present value of annuity due,
#' FV = future value, FVA = future value of annuity.
#' @examples TimeValue(interest.rate = .05, number.periods = 3, frequency = 1, type = "PV")
#' @export TimeValue
TimeValue <- function(interest.rate = numeric(), 
                      number.periods = numeric(), 
                      frequency = numeric(), 
                      type = "character"){
  if (missing(interest.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(interest.rate)  )
    stop("No numeric interest.rate specified.")
  if (interest.rate < 0 | interest.rate > 1)
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
  
  if(identical(type %in% c("PV", "PVA", "PVAD", "FV", "FVA"), FALSE))
    stop("Time value function not specfied correctly")
  
  interest.rate = interest.rate/frequency
  
  switch(type,
         PV =  1/(1+interest.rate)^number.periods,
         PVA = ((1-(1/(1+interest.rate)^number.periods))/interest.rate),
         PVAD =   ((1-(1/(1+interest.rate)^number.periods))/interest.rate) * (1+interest.rate),  
         FV =  (1+interest.rate)^number.periods,
         FVA =   (((1 + interest.rate)^(number.periods)) -1)/interest.rate)
  
}

setGeneric("TimeValue", function(interest.rate = numeric(), 
                                 number.periods = numeric(), 
                                 frequency = numeric(), 
                                 type = "character")
  {standardGeneric("TimeValue")})