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
# Bond Yield to Maturity Functions.  Basic function for
# yield and price of a non callable bond 
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