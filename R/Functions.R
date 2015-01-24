# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

# ----------------------------------
# Bond Yield to Maturity Functions 
# ----------------------------------

#YYTMtoPrice is a simple yield to price equation for a standard bond
#This equation treats the bond as a annuity and zero coupon payment
YTMtoPrice<- function(yield.to.maturity = numeric() ,coupon = numeric(), coupon.frequency = numeric(), 
                      years.mat = numeric(), face.value = numeric()){
  
  if (missing(yield.to.maturity))
    stop("Need to specify a maturity greater than 0")
  if (!is.numeric(yield.to.maturity)  )
    stop("No numeric yield to maturity specified.")
  if (yield.to.maturity <0 | yield.to.maturity > 1)
    stop("No valid  interest.rate specified.")
  
  #need to error trap this function
  n = years.mat * coupon.frequency
  c = coupon/coupon.frequency
  i = yield.to.maturity/coupon.frequency
  fv = face.value
  
  (((1-(1/(1+i)^n))/i) * (fv * c)) + (1/(1+i)^n * fv)   
}

#bondprice is a simple price to yield equation for a standard bond
#This equation treats the bond as a annuity and zero coupon payment
bondprice<- function(yield.to.maturity = numeric(),coupon = numeric(), coupon.frequency = numeric(), years.mat = numeric(), face.value = numeric()){
  n = years.mat * coupon.frequency
  c = coupon/coupon.frequency
  i = yield.to.maturity/coupon.frequency
  fv = face.value
  
  (((1-(1/(1+i)^n))/i) * (fv * c)) + (1/(1+i)^n * fv)
}

#bond estimated yield to maturity this equation estimates a bond's yield
#to maturity for a yield to maturity guess may be used with uniroot ytm functions
EstimYTM <- function(coupon = numeric(), coupon.frequency = numeric(), years.mat = numeric(), face.value = numeric(), price = numeric()){
  c = coupon
  n = years.mat
  f = coupon.frequency
  fv = face.value
  p = price/100 
  ((c * fv) + ((fv - (fv *p))/2)) / (((fv + (fv *p))/f))
}

#---------------------------
# Mortgage Payment Functions
#---------------------------

Mortgage.Monthly.Payment <- function(orig.bal = numeric(), note.rate = numeric(), term.mos = numeric()){
  
  #Error Trap Note Rate
  if (missing(note.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(note.rate)  )
    stop("No numeric interest.rate specified.")
  
  note.rate = note.rate/12 
  term = term.mos
  pmt.factor = (1+note.rate)^term
  pmt = (orig.bal * pmt.factor) * (note.rate/(pmt.factor -1))
  pmt
}

Sched.Prin <- function(balance = numeric(), note.rate = numeric(), term.mos = numeric(), period = numeric()){
  note.rate = note.rate/12
  term = term.mos
  disc.pmt =  note.rate * (1+note.rate)^(period-1)
  disc.prin = ((1+note.rate)^(term))-1
  Scheduled.Prin = balance *(disc.pmt/disc.prin)
  Scheduled.Prin
}

Remain.Balance <- function(balance = numeric(), note.rate = numeric(), term.mos = numeric(), period = numeric()){
  note.rate = note.rate/12
  term = term.mos
  Remain.Balance = balance * ((((1+note.rate)^term) - ((1+note.rate)^period))/(((1+note.rate)^term)-1))
  Remain.Balance
}

PPC.Ramp <- function(season.period = numeric(), begin.cpr = numeric(), end.cpr = numeric(), period = numeric()){
  if(end.cpr >= 1) {end.cpr = end.cpr/100 
                    begin.cpr = begin.cpr/100}
  monthly.cpr = (begin.cpr + ((period - 1) * (end.cpr-begin.cpr)/(season.period -1)))
  cpr = ifelse(monthly.cpr <= end.cpr, monthly.cpr, end.cpr)
  cpr
}

SMM.To.CPR <- function(SMM = numeric()){
  if (missing(SMM))
    stop("Need to specify a SMM Value")
  if (!is.numeric(SMM)  )
    stop("No numeric SMM specified.")
  if (SMM <0 | SMM > 1)
    stop("No SMM specified.")
  SMM = 1-((1-SMM)^(12))
  return(SMM)
}

CPR.To.SMM <- function(CPR = numeric()){
  if (missing(CPR))
    stop("Need to specify a SMM Value")
  if (!is.numeric(CPR)  )
    stop("No numeric SMM specified.")
  if (CPR <0 | CPR > 1)
    stop("No SMM specified.")
  CPR = 1-((1-CPR)^(1/12))
  return(CPR)
}

SMMVector.To.CPR <- function(SMM = vector(), num.period = numeric()){
  # This function yields the average SMM  
  SMM = prod(1 + SMM)^(1/num.period)
  SMM = SMM - 1
  SMMVector.to.CPR = 1-(1-SMM)^12 
}

#-------------------------------------
#Risk measures.  These functions measure effective duration, effective convexity, and key rate duration
#-------------------------------------
Effective.Duration <- function(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds){
  Price = proceeds/10
  Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)  
  (Price.UP - Price.DWN)/(2*Price*Rate.Delta)
}

Effective.Convexity <- function(Rate.Delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds){
  Price = proceeds/10
  Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
  Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
  Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
  
  (Price.UP + Price.DWN + (2*Price))/(2*(Price*Rate.Delta)^2)
}


