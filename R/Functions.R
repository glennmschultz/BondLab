# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 
# ==========================================================================================


#' Determine the price a bond on the interest payment date 
#' 
#' This is generic function to determine the price of a bond
#' given the yield to maturity (YTM).  It is a nominal example of 
#' pricing a bond given its yield to maturity.  The equation assumes pricing
#' from one payment date to the next.  It does not account for acrrued interest.
#' @param yield.to.maturity A numeric value expressing the yield to
#' maturity (discount rate) as an annual percentage.
#' @param coupon A numeric value the coupon paid by the bond as a percentage of
#' the bond's principal amount
#' @param coupon.frequency A numeirc value expressing the frequency of payments
#' over one year
#' @param years.mat A numeric value expressing the years to maturity
#' @param face.value A numeric value expressing the face value (principal amount) of the bond
#' @examples bondprice(yield.to.maturity = .05, coupon = .05, coupon.frequency = 2,
#' years.mat = 10, face.value = 100)
#' @export bondprice
  bondprice<- function(yield.to.maturity = numeric(),
                     coupon = numeric(), 
                     coupon.frequency = numeric(), 
                     years.mat = numeric(), 
                     face.value = numeric()){
  
  n = years.mat * coupon.frequency
  c = coupon/coupon.frequency
  i = yield.to.maturity/coupon.frequency
  fv = face.value
  
  (((1-(1/(1+i)^n))/i) * (fv * c)) + (1/(1+i)^n * fv)
  }

  setGeneric("bondprice", function(yield.to.maturity = numeric(),
                                 coupon = numeric(), 
                                 coupon.frequency = numeric(), 
                                 years.mat = numeric(), 
                                 face.value = numeric())
  {standardGeneric("bondprice")})

#' A function to estimate the yield to maturity of a standard bond
#' 
#' Estimate a bond's yield to maturity given a price.  It is a 
#' nominall example and does not account for acrrued interest
#' @param coupon A numeric value expressing the bond's coupon as a
#' percentage of its face amount
#' @param coupon.frequency A numeric value expressing the frequency of payments
#' over one year
#' @param years.mat A numeric value expressing the years to maturity
#' @param face.value A numeric value expressing the face value (principal amount) of the bond
#' @param price A numeric value expressing the price of the bond (not percentage of face value)
#' for example a price of$102 is entered as 102.00
#' @examples EstimYTM(coupon = .04, coupon.frequency = 2, years.mat = 10, face.value = 1000, price = 100)
#' @export EstimYTM
  EstimYTM <- function(coupon = numeric(), 
                     coupon.frequency = numeric(), 
                     years.mat = numeric(), 
                     face.value = numeric(), 
                     price = numeric()){
  c = coupon
  n = years.mat
  f = coupon.frequency
  fv = face.value
  p = price/100 
  ((c * fv) + ((fv - (fv *p))/2)) / (((fv + (fv *p))/f))
  }

  setGeneric("EstimYTM", function(coupon = numeric(), 
                                coupon.frequency = numeric(), 
                                years.mat = numeric(), 
                                face.value = numeric(), 
                                price = numeric())
  {standardGeneric("EstimYTM")})

#' A function to calculate the monthly payment of a mortgage
#' 
#' A standard generic function to compute the monthly payment of a mortgage
#'@param orig.bal A numeric value, the original loan amount
#'@param note.rate A numeric vlaue, the borrower's note rate
#'@param term.mos A numeric value, the original term of the loan in months
#'@examples Mortgage.Monthly.Payment(orig.bal = 100000, note.rate = .04, term.mos = 360)
#' @export Mortgage.Monthly.Payment
  Mortgage.Monthly.Payment <- function(orig.bal = numeric(), 
                                     note.rate = numeric(), 
                                     term.mos = numeric()){
  #This function computes the monthly mortgage payment and can be used as part
  #Error Trap note rate.
  if (missing(note.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(note.rate)  )
    stop("No numeric interest.rate specified.")
  
  note.rate = note.rate/months.in.year
  term = term.mos
  pmt.factor = (1+note.rate)^term
  pmt = (orig.bal * pmt.factor) * (note.rate/(pmt.factor -1))
  return(pmt)

  }

  setGeneric("Mortgage.Monthly.Payment", function(orig.bal = numeric(),
                                                note.rate = numeric(),
                                                term.mos = numeric())
  {standardGeneric("Mortgage.Monthly.Payment")})

#' A function to calculate the scheduled principal of mortgage
#' 
#' A standard generic for the calculation of scheduled principal of mortgage loan
#' @param balance A numeric value, the balance of the mortgage
#' @param note.rate A numeric value, the borrower's note rate
#' @param term.mos A numeric value, the term of the mortgage
#' @param period A numeric value the number of months since the first payment
#' @examples Sched.Prin(balance = 100000, note.rate = .045, term.mos = 360, period = 2)
#' @export Sched.Prin
  Sched.Prin <- function(balance = numeric(), 
                       note.rate = numeric(), 
                       term.mos = numeric(), 
                       period = numeric()){
  
  #This function computes the monthly scheduled principal 
  #Requires additional error trapping
  note.rate = note.rate/months.in.year
  term = term.mos
  disc.pmt =  note.rate * (1+note.rate)^(period-1)
  disc.prin = ((1+note.rate)^(term))-1
  Scheduled.Prin = balance *(disc.pmt/disc.prin)
  return(Scheduled.Prin)
  }

  setGeneric("Sched.Prin", function(balance = numeric(),
                                  note.rate = numeric(),
                                  term.mos = numeric(),
                                  period = numeric())
  {standardGeneric("Sched.Prin")})

#' A function to compute the remaining balance of a mortgage
#' 
#' A standard generic for the calculation of a mortgage remaining balance
#' @param balance A numeric value the balance of the mortgage
#' @param note.rate A numeric value the borrower's note rate
#' @param term.mos A numeric value the term of the mortgage
#' @param period A numeric value the numeric of months from the first payment
#' @examples Remain.Balance(balance = 100000, note.rate = .045, term.mos = 360, period = 10)
#' @export Remain.Balance
  Remain.Balance <- function(balance = numeric(), 
                           note.rate = numeric(), 
                           term.mos = numeric(), 
                           period = numeric()){
  note.rate = note.rate/months.in.year
  term = term.mos
  Remain.Balance = balance * ((((1+note.rate)^term) - ((1+note.rate)^period))/(((1+note.rate)^term)-1))
  Remain.Balance
  }

  setGeneric("Remain.Balance", function(balance = numeric(), 
                                        note.rate = numeric(), 
                                        term.mos = numeric(), 
                                        period = numeric())
  {standardGeneric("Remain.Balance")})

#' A function to calculate the PPC ramp
#' 
#' A standard generic function for the calculation of the PPC ramp
#' @param season.period A numeric value the time to the peak PPC ramp
#' @param begin.cpr A numeric value the starting CPR value
#' @param end.cpr A numeric value the ending CPR value
#' @param period A numeric value the current period (months from start date)
#' @examples PPC.Ramp(season.period = 30, begin.cpr = .002, end.cpr = .06, period = 30) 
#' @export PPC.Ramp
  PPC.Ramp <- function(season.period = numeric(), 
                     begin.cpr = numeric(), 
                     end.cpr = numeric(), 
                     period = numeric()){
  
  if(end.cpr >= 1) {end.cpr = end.cpr/100 
                    begin.cpr = begin.cpr/100}
  monthly.cpr = (begin.cpr + ((period - 1) * (end.cpr-begin.cpr)/(season.period -1)))
  cpr = ifelse(monthly.cpr <= end.cpr, monthly.cpr, end.cpr)
  return(cpr)
  }

  setGeneric("PPC.Ramp", function(season.period = numeric(),
                                begin.cpr = numeric(),
                                end.cpr = numeric(),
                                period = numeric())
  {standardGeneric("PPC.Ramp")})

#' Convert the single monthly mortality rate SMM to CPR
#' 
#' A standard generic function used to convert the SMM to CPR
#' @param SMM A numeric value the single monthly mortality rate (SMM)
#' @examples SMM.To.CPR(SMM = .002)
#' @export SMM.To.CPR
  SMM.To.CPR <- function(SMM = numeric()){
  if (missing(SMM))
    stop("Need to specify a SMM Value")
  if (!is.numeric(SMM)  )
    stop("No numeric SMM specified.")
  if (SMM <0 | SMM > 1)
    stop("No SMM specified.")
  SMM = 1-((1-SMM)^(months.in.year))
  return(SMM)
  }

setGeneric("SMM.To.CPR", function(SMM = numeric())
  {standardGeneric("SMM.To.CPR")})

#' A function to convert the CPR to a single monthly mortality rate SMM
#' 
#' Standard Generic Function used to convert CPR to SMM 
#' @param CPR A numeric value
#' @examples
#' CPR.To.SMM(CPR = .06)
#' @export CPR.To.SMM
  CPR.To.SMM <- function(CPR = numeric()){
  if (missing(CPR))
    stop("Need to specify a SMM Value")
  if (!is.numeric(CPR)  )
    stop("No numeric SMM specified.")
  if (CPR <0 | CPR > 1)
    stop("No SMM specified.")
  CPR = 1-((1-CPR)^(1/months.in.year))
  return(CPR)
  }

  setGeneric("CPR.To.SMM", function(CPR = numeric())
  {standardGeneric("CPR.To.SMM")})

  #' A function to convert  a time series of SMM to CPR
  #' 
  #' A standard generic function for the conversion a SMM time series to CPR
  #' to a time series of CPR.
  #' @param SMM A numeric vector of SMM
  #' @param num.period a numeric vector of period (Loan Age)
  #' @export SMMVector.To.CPR
  SMMVector.To.CPR <- function(SMM = vector(), 
                             num.period = vector()){
  # This function yields the average SMM  
  SMM = prod(1 + SMM)^(1/num.period)
  SMM = SMM - 1
  SMMVector.to.CPR = 1-(1-SMM)^months.in.year 
  }

  setGeneric("SMMVector.To.CPR", function(SMM = vector(), num.period = vector())
    {standardGeneric("SMMVector.To.CPR")})

#' A function to compute effective duration
#' 
#' Calculates the effective duration based on dscount vector (zero coupon)
#' cashflow vector, and rate delta
#' @param Rate.Delta A numeric value the interest rate shift in basis points
#' @param cashflow A numeric vector of cashflow
#' @param discount.rates A numeric vector of the discount rates
#' @param time.period A numeric vector of the time period
#' @export
  Effective.Duration <- function(Rate.Delta = numeric(), 
                               cashflow = vector(), 
                               discount.rates = vector(), 
                               time.period = vector()){

  discount.rates.up = discount.rates + Rate.Delta
  discount.rates.dwn = discount.rates - Rate.Delta
  Price.NC = sum((1/((1+discount.rates)^time.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^time.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^time.period)) * cashflow)  
  (Price.UP - Price.DWN)/(2*Price.NC*Rate.Delta)
  }
  
  setGeneric("Effective.Duration", function(Rate.Delta = numeric(), 
                                            cashflow = vector(), 
                                            discount.rates = vector(), 
                                            time.period = vector())
    {standardGeneric("Effective.Duration")})

#' A function to compute effective convexity
#' 
#' Calculates effective convexity based on a discount vector (zero coupon)
#' cashflow vector, and rate delta
#' @param Rate.Delta  A numeric value the interest rate shift in basis points
#' @param cashflow A numeric vector of cashflow
#' @param discount.rates A numeric vector of the up discount rates
#' @param time.period A numeric vector of the down discount rates
#' @export Effective.Convexity
  Effective.Convexity <- function(Rate.Delta = numeric(), 
                                 cashflow = vector(), 
                                 discount.rates = vector(),
                                 time.period = vector()){
  discount.rates.up = discount.rates + Rate.Delta
  discount.rates.dwn = discount.rates - Rate.Delta
  Price.NC = sum((1/((1+discount.rates)^time.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^time.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^time.period)) * cashflow)
  
  (Price.UP + Price.DWN - (2*Price.NC))/(2*Price.NC*(Rate.Delta^2))
  }
  
  
  setGeneric("Effective.Convexity", function(Rate.Delta = numeric(), 
                                            cashflow = vector(), 
                                            discount.rates = vector(), 
                                            time.period = vector())
  {standardGeneric("Effective.Convexity")})


