# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#--------------------------
#Bond cash flow function. This function computes the cash flow of a standard non-callable bond
#-------------------------
BondCashFlows <- function (bond.id = "character", principal = numeric(), settlement.date = "character", price = numeric()){
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency       
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  
  # This function error traps bond input information
  ErrorTrap(bond.id = bond.id, principal = principal, settlement.date = settlement.date, price = price)
  
  #  Validate the price and coupon passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(price <= 1) {price = price} else {price = price/100}
  if(coupon > 1) {coupon = coupon/100} else {coupon = coupon}
  
  #Calculate the number of cashflows that will be paid from settlement date to maturity date 
  #step1 calculate the years to maturity  
  ncashflows = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                   lastpmt.date = lastpmt.date, nextpmt.date = end.date) 
  
  #Step2 build a vector of dates for the payment schedule
  # first get the pmtdate interval
  pmtdate.interval = 12/frequency
  # then compute the payment dates
  pmtdate = as.Date(c(if(settlement.date == issue.date) {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
                      else {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y")
  
  #step3 build the time period vector (n) for discounting the cashflows nextpmt date is vector of payment dates to n for each period
  time.period = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, nextpmt.date = pmtdate)
  
  #step4 Count the number of cashflows 
  #num.periods is the total number of cashflows to be received
  #num.period is the period in which the cashflow is received
  num.periods = length(time.period)
  col.names <- c("Period", "Date", "Time", "Principal Outstanding", "Coupon", "Coupon Income", "Principal Paid", "TotalCashFlow",
                 "Present Value Factor", "Present Value", "Duration", "Convexity Time", "CashFlow Convexity", "Convexity")
  
  Bond.CF.Table <- array(data = NA, c(num.periods, 14), dimnames = list(seq(c(1:num.periods)),col.names))  
  for(i in 1:num.periods){
    Bond.CF.Table[i,1] = i
    Bond.CF.Table[i,2] = pmtdate[i]
    Bond.CF.Table[i,3] = time.period[i]
    Bond.CF.Table[i,4] = principal
    Bond.CF.Table[i,5] = coupon /frequency
    Bond.CF.Table[i,6] = Bond.CF.Table[i,5] * Bond.CF.Table[i,4]
    if(Bond.CF.Table[i,2] == end.date) {Bond.CF.Table[i,7] = principal} else {Bond.CF.Table[i,7] = 0}
    Bond.CF.Table[i,8] = Bond.CF.Table[i,6] + Bond.CF.Table[i,7]
  }
  
  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, 
                                         settlement.date = settlement.date, lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date)) * 360
  
  days.between.pmtdate = ((12/frequency)/12) * 360
  days.of.accrued = days.between.pmtdate - days.to.nextpmt
  accrued.interest = (days.of.accrued/days.between.pmtdate) * Bond.CF.Table[1,6]
  
  #Step6 solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  #it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value 
  irr <- function(rate , time.period , cashflow , principal , price , accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol =.000000001, time.period = Bond.CF.Table[,3], 
                cashflow = Bond.CF.Table[,8], principal = principal, price = price, accrued.interest = accrued.interest)$root
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency
  
  #Step7 Present value of the cash flows Present Value Factors
  Bond.CF.Table[,9] = 1/((1+(Yield.To.Maturity/frequency))^(Bond.CF.Table[,3] * frequency))
  
  #Present Value of the cash flows
  Bond.CF.Table[,10] = Bond.CF.Table[,8] * Bond.CF.Table[,9]
  
  #Step8 Risk measures Duration Factors
  Bond.CF.Table[,11] = Bond.CF.Table[,3] * (Bond.CF.Table[,10]/((principal * price) + accrued.interest))
  
  #Convexity Factors
  Bond.CF.Table[,12] = Bond.CF.Table[,3] *(Bond.CF.Table[,3] + 1)
  Bond.CF.Table[,13] = (Bond.CF.Table[,8]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((Bond.CF.Table[,3] + 2) * frequency)))/((principal * price) + accrued.interest)
  Bond.CF.Table[,14] = Bond.CF.Table[,12] * Bond.CF.Table[,13] 
  
  #Weighted Average Life
  WAL = sum((Bond.CF.Table[,7] * Bond.CF.Table[,3]))/sum(Bond.CF.Table[,7])
  #Duration and Convexity
  Duration = apply(Bond.CF.Table, 2, sum)[11]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(Bond.CF.Table, 2, sum)[14] * .5
  
  #Assign Values to the slots
  new("BondCashFlows",   
      Price = price * 100,
      Accrued = accrued.interest,
      YieldToMaturity = Yield.To.Maturity,
      WAL = WAL,
      ModDuration = Modified.Duration,
      Convexity = Convexity,
      Period = Bond.CF.Table[,1],
      PmtDate = as.character(as.Date(Bond.CF.Table[,2], origin = "1970-01-01")),
      TimePeriod = Bond.CF.Table[,3],
      PrincipalOutstanding  = Bond.CF.Table[,4],
      CouponPmt = Bond.CF.Table[,5],
      TotalCashFlow = Bond.CF.Table[,8],
      bond.id
  )
}


# -----------
# Bond Key Rate Calculation
# -------------
BondTermStructure <- function(bond.id = "character", Rate.Delta = numeric(), TermStructure = "character", principal = numeric(), price = numeric(), cashflow = "character"){
  
  #Call the bond frequency to adjust the spot spread to the payment frequency of the bond
  frequency = bond.id@Frequency
  maturity = bond.id@Maturity
  accrued = cashflow@Accrued
  
  #Class name variable.  This will set the class name for the new class to be initilized
  ClassName <- if(bond.id@BondType != "MBS") {as.character("BondTermStructure")} else {as.character("MortgageTermStructure")}
  
  #Error Trap the user's price input
  if(price <= 1) {price = price} else {price = price/100}
  if(price <=0) stop("No valid bond price")
  proceeds = (principal * price) + accrued 
  
  #========== Set the functions that will be used ==========
  # These functions are set as internal functions to key rates
  # this insures that stored values will not be wrongly be passed to the funtion
  # internal functions used to compute key rate duration and convexity
  EffectiveMeasures <- function(rate.delta, cashflow, discount.rates, discount.rates.up, discount.rates.dwn, 
                                t.period, proceeds, type){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
    
    switch(type, 
           duration = (Price.UP - Price.DWN)/(2*proceeds*rate.delta),
           convexity =  (Price.UP + Price.DWN - (2*proceeds))/(2 * proceeds * rate.delta^2)
    )
  }
  
  Effective.Duration <- function(rate.delta, cashflow, discount.rates, 
                                 discount.rates.up, discount.rates.dwn, t.period, proceeds){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
    (Price.UP - Price.DWN)/(2*proceeds*rate.delta)
  }
  Effective.Convexity <- function(rate.delta, cashflow, discount.rates, 
                                  discount.rates.up, discount.rates.dwn, t.period, proceeds){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow)
    (Price.UP + Price.DWN - (2*proceeds))/(2*proceeds *(rate.delta^2))
  }
  
  #The spot spread function is used to solve for the spread to the spot curve to normalize discounting
  Spot.Spread <- function(spread = numeric(), cashflow = vector(), discount.rates = vector(), 
                          t.period = vector(), proceeds = numeric()){
    Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
    return(proceeds - Present.Value)
  }
  
  #================= set up the index names for each array that will be used in the function
  # Index names set names for columns in the KRIndex. This table set the control strucutre for 
  # the loop that will compute key rate duration given rates in the key rate table
  Index.Names <- c("Period", "Time", "Spot Curve", "Disc Curve", "KRDwn", "KRUp")
  
  # KR.Duration.Col set the column names for the table that hold the key rate results
  # this table will output to class bond analytics slot KRTenor and KRDuration
  KR.Duration.Col <- c("Key Rate", "Key Rate Duration", "Key Rate Convexity")
  
  #sets the tenor of the key rate that will report a duration
  KR.Duration.Row <- c("0.25", "1", "2", "3", "5", "7", "10", "15", "20", "25", "30")
  
  #set the arrays for key rate duration calculation
  #key rate table holds data for the term structure and shifts in the key rates
  #!!!!!!!!!!!!!! DIM TO LENGTH OF CASH FLOW ARRAY AND SET LAST KR TO LENGTH LINE 604
  Key.Rate.Table <- array(data = NA, c(360,6), dimnames = list(seq(c(1:360)), Index.Names))
  
  #key rate duration array holds the key rates and the key rate duration
  KR.Duration <- array(data = NA, c(11,3), dimnames = list(seq(c(1:11)), KR.Duration.Col))
  KR.Duration[,1] <- as.numeric(KR.Duration.Row)
  
  # Create Index for Key Rate Table for interpolation of Key Rate Duration set outer knot points
  # the outer points are the first and last elements in KR string
  # this needs some logic to change the right knot point if the maturity or last payment of the 
  # bond is greater than 30-years should be adaptive
  KR <- c("0.083", "0.25", "1", "2", "3", "5", "7", "10", "15", "20", "25", "30", "30")   # Key Rates
  KRCount = length(KR)
  KRIndex <- array(data = NA, c(KRCount, 6), dimnames = list(seq(c(1:KRCount)), Index.Names))
  
  # Initialize the cash flow array for discounting and key rate caclulations
  # this will be populated from class BondCashFlows
  # !!!!!!!!!!!!!!!!!DIM CASHFLOW ARRAY TO SIZE OF CASHFLOW!!!!!!!!!!!!!!!!!!!!!!
  CashFlowArray <- array(data = NA, c(360,2), 
                         dimnames = list(seq(1:360), c("period", "cashflow")))
  
  #Initialze the spot rate array for key rate duration calculations
  SpotRate <- as.matrix(TermStructure@spotrate)
  
  # Populate Period, Time(t) and Spot Rate Curve of Key Rate Table using NS coefficients from Term Stucture
  # and then populate and align the cashflow array for discounting and key rate computations
  # !!!!!!!!!!!!!!! SET LOOP TO LENGTH OF CASHFLOW ARRAY !!!!!!!!!!!!!!!!!!!!!!!!!!
  for(x in 1:360){
    
    #Period (n) in which the cashflow is received
    Key.Rate.Table[x,1] = x
    
    # Time (t) at which the cashflow is received
    #Time period in which the cashflow was received for discounting
    Key.Rate.Table [x,2] = x/12
    
    #spot rates for discounting
    Key.Rate.Table[x,3] = SpotRate[x,1]/100   
    
    #Align Cash Flows and populated the CashFlowArray
    #Step One: Make sure all cash flows are set to zero
    CashFlowArray[x,1] = Key.Rate.Table[x,2]
    CashFlowArray[x,2] = 0
  }
  
  #Step Two: Initialize loop and set the cashflows in the array
  #This loops through the time period and set the cashflows into the propery array location for
  #discounts by indexing the cashflows to the array.  The indexing is conditional on the integer of the first period less than or equal to 1
  
  if(as.integer(cashflow@TimePeriod[1] *12) != 1) CashFlowArray[as.integer(cashflow@TimePeriod * 12) + 1,2] = cashflow@TotalCashFlow
  if(as.integer(cashflow@TimePeriod[1] * 12) == 1) CashFlowArray[as.integer(cashflow@TimePeriod * 12),2] = cashflow@TotalCashFlow
  
  #solve for spread to spot curve to equal price
  spot.spread <- uniroot(Spot.Spread, interval = c(-.75, .75), tol = .0000000001, CashFlowArray[,2],
                         discount.rates = Key.Rate.Table[,3], t.period = Key.Rate.Table[,2] , proceeds)$root
  
  
  #convert the spot spread to the frequency of the bond
  #spot.spread = (((1+spot.spread)^(1/frequency))-1) * frequency
  
  #Step three add the spot spread to the spot curve to get the discount rates that are need for
  #the key rate duration calculation
  
  #at a minimum the cash flow array should be 360 months
  for(i in 1:360){
    Key.Rate.Table[i,4] = Key.Rate.Table[i,3] + spot.spread                                  
  }
  
  #========= Populate KRIndex Table =========================
  # The key rate index table will serve as the control table for the looping
  # using this table allows for incremental looping of discontinous segments of the
  #spot rate curve and is proprietary to bondlab
  # Step 1 populate Period (n)
  KRIndex[1:KRCount,1] <- round(as.numeric(KR) *12,0)
  
  # Step 2 populate time period (t)
  KRIndex[1:KRCount,2] <- as.numeric(KR)                    
  
  # Step 3 Populate Index Table with the relevant points on the spot curve
  # this is done by looping through the key rate table and allows for term structure implementation
  # other than Nelson Siegel (note: this capability needs to built into bondlab)
  # the key rate index table (KRIndex) is used to populate the key rate table (KRTable)
  for (j in 1:KRCount){                                   
    for (i in 1:360){
      if (Key.Rate.Table[i,1] == round(KRIndex[j,2] * 12,0)) {KRIndex[j,3] = Key.Rate.Table[i,3]} else {KRIndex[j,3] = KRIndex[j,3]}
    }
  }
  
  # Step 4 Populate KRIndex Table with the appropriate Discount Curve values from the key rate table 
  # these will be the reference points for the appropriate key rate shifts
  for (j in 1:KRCount){                                   
    for (i in 1:360){
      if (Key.Rate.Table[i,1] == round(KRIndex[j,2] * 12,0)) {KRIndex[j,4] = Key.Rate.Table[i,4]} else {KRIndex[j,3] = KRIndex[j,3]}
    }
  }
  
  # Step 5 Populated KRIndex Table with KR Shifts
  for (j in 1:KRCount){
    KRIndex[j,5] = KRIndex[j,4] - (Rate.Delta/100)
    KRIndex[j,6] = KRIndex[j,4] + (Rate.Delta/100)
  }
  
  #===== Implement Shift of Spot Rates =======================
  # Once the KRIndex is populated implement the shift in the spot rates using the KRIndex table as the control 
  # w is the counter of the internal knot points used to compute key rate duration it ignores the boundary knots
  # used for interpolation at the end points.  x is the length of the array.  Currently the analysis is limited
  # to loans (bonds) with a maximum of 30-years to maturity.  This can be made dynamic at some point in the future
  # y is column counter used the key rate down and key rate up values
  for (w in 2:(KRCount-1)){ 
    for (x in 1:360){
      for(y in 5:6){
        
        # step 1: populate the spot curve outside the key rate shift =========
        if(Key.Rate.Table[x,2] <= KRIndex[w-1,2] || Key.Rate.Table[x,2] >= KRIndex[w+1,2]) 
        {Key.Rate.Table[x,y] = Key.Rate.Table[x,4]} else {Key.Rate.Table[x,y] = 0}
      }
    }
    
    #===== Begin Interpolation of Spot Curve ==================================
    # Maturity points on the spot rate curve to interpolate
    KRx <- c(KRIndex[w-1,2], KRIndex[w,2], KRIndex[w+1,2]) 
    
    # Spot rates that correspond to the maturity points Down and Up
    for(z in 1:2){                                       
      if (z == 1) 
      {KRy <- c(KRIndex[w-1,4], KRIndex[w,5], KRIndex[w+1,4])}         
      else 
      {KRy <- c(KRIndex[w-1,4], KRIndex[w,6], KRIndex[w+1,4])}                                     
      a = KRIndex[w-1,1]+ 1
      b = KRIndex[w+1,1] - 1
      for(h in a : b){
        Key.Rate.Table[h,(z+4)] <- approx(KRx,KRy, Key.Rate.Table[h,2])$y
      } # Loop through Key Rate Table and interpolation
    } # Inner Loop to set interpolation points from KRIndex
    
    # This line sets the end points for disocunting when the 30-year is last point on the curve
    # It is possible to set the endpoints longer using row 12 of the KRIndex but this will have to wait
    # the cash flow table is programmed for MBS
    if (KRIndex[w,2] == 30) {(Key.Rate.Table[x,5] = KRIndex[12,5]) & (Key.Rate.Table[x,6] = KRIndex[12,6])}   
    
    #============================== Calculate Key Rate Duration ============================================
    KR.Duration[w-1,2] <- -EffectiveMeasures(
      rate.delta = Rate.Delta/100, 
      cashflow = CashFlowArray[,2], 
      discount.rates = Key.Rate.Table[,4], 
      discount.rates.up = Key.Rate.Table[,6],
      discount.rates.dwn = Key.Rate.Table[,5],
      t.period = Key.Rate.Table[,2],
      type = "duration",
      proceeds = proceeds
    ) 
    KR.Duration[w-1,3] <- EffectiveMeasures(
      rate.delta = Rate.Delta/100, 
      cashflow = CashFlowArray[,2], 
      discount.rates = Key.Rate.Table[,4], 
      discount.rates.up = Key.Rate.Table[,6],
      discount.rates.dwn = Key.Rate.Table[,5],
      t.period = Key.Rate.Table[,2],
      type = "convexity",
      proceeds = proceeds
    ) 
  } # Outer Loop around KRIndex
  new(ClassName,
      bond.id,
      SpotSpread = spot.spread * 100,
      EffDuration = sum(KR.Duration[,2]),
      EffConvexity = sum(KR.Duration[,3]),
      KeyRateTenor = KR.Duration[,1],
      KeyRateDuration = KR.Duration[,2],
      KeyRateConvexity = KR.Duration[,3]
      
  )
} # End the function

#---------------------------------------------------
#Term strucutre call term strc 
#and holds forward and spot rates as slots to class Term Structure 
#---------------------------------------------------
TermStructure <- function(rates.data = "character", method = "character"){
  
  #function(trade.date = "character", method = "character")  
  #Error Trap User inputs to the function
  if(missing(rates.data)) stop("missing rates data object")  
  
  #Default to Nelson-Siegel
  if(missing(method)) method = "ns"
  
  #Default to parametric
  if(method == "cs") stop("cubic spline not implemented")
  
  #Check that the user input a valid method
  CheckMethod <- c("ns", "dl", "sv", "asv", "cs")
  if(!method %in% CheckMethod) stop ("Invalid 'method' Value")
  
  # pass the yield curve to the function
  rates.data <- rates.data
  
  #set the column counter to make cashflows for termstrucutre
  ColCount <- as.numeric(ncol(rates.data))
  Mat.Years <- as.numeric(rates.data[2,2:ColCount])
  Coupon.Rate <- as.numeric(rates.data[1,2:ColCount])
  Issue.Date <- as.Date(rates.data[1,1])
  
  #initialize coupon bonds S3 class
  #This can be upgraded when bondlab has portfolio function
  ISIN <- vector()
  MATURITYDATE <- vector()
  ISSUEDATE <- vector()
  COUPONRATE <- vector()
  PRICE <- vector()
  ACCRUED <- vector()
  CFISIN <- vector()
  CF <- vector()
  DATE <- vector()
  CASHFLOWS  <- list(CFISIN,CF,DATE)
  names(CASHFLOWS) <- c("ISIN","CF","DATE")
  TODAY <- vector()
  data <- list()
  TSInput <- list()
  
  ### Assign Values to List Items #########
  data = NULL
  data$ISIN <- colnames(rates.data[2:ColCount])
  data$ISSUEDATE <- rep(as.Date(rates.data[1,1]),ColCount - 1)
  
  data$MATURITYDATE <-
    sapply(Mat.Years, function(Mat.Years = Mat.Years, Issue = Issue.Date) 
    {Maturity = if(Mat.Years < 1) {Issue %m+% months(round(Mat.Years * months.in.year))} else 
    {Issue %m+% years(as.numeric(Mat.Years))}
    return(as.character(Maturity))
    }) 
  
  data$COUPONRATE <- ifelse(Mat.Years < 1, 0, Coupon.Rate)                  
  
  data$PRICE <-      ifelse(Mat.Years < 1, (1 + (Coupon.Rate/100))^(Mat.Years * -1) * 100, 100)
  
  data$ACCRUED <- rep(0, ColCount -1)
  
  for(j in 1:(ColCount-1)){
    Vector.Length <- as.numeric(round(difftime(data[[3]][j],
                                               data[[2]][j],
                                               units = c("weeks"))/weeks.in.year,0))
    Vector.Length <- ifelse(Vector.Length < 1, 1, Vector.Length * pmt.frequency)  #pmt.frequency should be input 
    data$CASHFLOWS$ISIN <- append(data$CASHFLOWS$ISIN, rep(data[[1]][j],Vector.Length))
    data$CASHFLOWS$CF <- append(data$CASHFLOWS$CF,as.numeric(c(rep((data[[4]][j]/100/pmt.frequency),Vector.Length-1) * min.principal, (min.principal + (data$COUPONRATE[j]/100/pmt.frequency)* min.principal))))
    by.months = ifelse(data[[4]][j] == 0, round(difftime(data[[3]][j], rates.data[1,1])/days.in.month), 6) # this sets the month increment so that cashflows can handle discount bills
    data$CASHFLOWS$DATE <- append(data$CASHFLOW$DATE,seq(as.Date(rates.data[1,1]) %m+% months(as.numeric(by.months)), as.Date(data[[3]][j]), by = as.character(paste(by.months, "months", sep = " "))))
    
  } #The Loop Ends here and the list is made
  
  data$TODAY <- as.Date(rates.data[1,1])
  TSInput[[as.character(rates.data[1,1])]] <- c(data)
  
  #set term strucuture input (TSInput) to class couponbonds
  class(TSInput) <- "couponbonds"
  
  #Fit the term structure of interest rates
  
  if(method != "cs") {TSFit <- estim_nss(dataset = TSInput, group = as.character(rates.data[1,1]), matrange = "all", method = method)} else
  {TSFit <- estim_cs(bonddata = TSInput, group = as.character(rates.data[1,1]), matrange = "all", rse = TRUE)}
  
  #Return the coefficient vector to be passed in to the spot and forward rate functions
  #Maybe have the method choosen based on the one that gives the smallest RMSE
  Vector <- switch(method,
                   ns = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1")]),
                   dl = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2")]),
                   sv = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1", "beta3", "tau2")]),
                   asv = unname(TSFit$opt_result[[1]]$par[c("beta0", "beta1", "beta2", "tau1", "tau2", "tau3")]),
                   #cs = need to figure this out
  )
  
  #Calculate the spot rate curve and determine the forward rates needed to 
  period <- seq(from = 1, to = 492, by = 1)
  #Use the date from the cashflow file
  date <- seq(as.Date(rates.data[1,1]) %m+% months(1), as.Date(data[[3]][j]), by="1 months")
  spot.rate.curve <- spotrates(method = method, beta = Vector, m = seq(from = 1/12, to = 492/12, by = 1/12))
  forward.rate.curve <- forwardrates(method = method, beta = Vector, m = seq(from = 1/12, to = 492/12, by = 1/12))
  Two.Year.Fwd <- (((1 + spot.rate.curve[seq(from = 25, to = 385, by = 1)]) ^ (period[seq(from = 25, to = 385, by = 1)]/12) /
                      (1 + spot.rate.curve[seq(from = 1, to = 361, by = 1)]) ^ (period[seq(from = 1, to = 361, by = 1)]/12))^(1/2))-1
  
  Ten.Year.Fwd <- (((1 + spot.rate.curve[seq(from = 121, to = 481, by = 1)]) ^ (period[seq(from = 121, to = 481, by = 1)]/12) /
                      (1 + spot.rate.curve[seq(from = 1, to = 361, by = 1)]) ^ (period[seq(from = 1, to = 361, by = 1)]/12))^(1/10))-1
  
  new("TermStructure",
      tradedate = as.character(rates.data[1,1]),
      period = as.numeric(period),
      date = as.character(date),
      spotrate = spot.rate.curve,
      forwardrate = forward.rate.curve,
      TwoYearFwd = Two.Year.Fwd,
      TenYearFwd = Ten.Year.Fwd
  )
} 


# --------------------------------
# Bond Analytics Functions - THESE ARE THE BOND LAB ENGINES !!!
# These functions are different from the above they use the functions together
# to analyze a bond or mortgage backed security using the above functions and construct the appropriate objects (classes)  
# --------------------------------

# This function analyzes a standard non callable bond and serves as the constructor function
# These are the engines  
# -----------------------------------
BondAnalytics <- function (bond.id = "character", principal = numeric(), price = numeric(), trade.date = "character", 
                           settlement.date = "character", method = method) 
{
  
  #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) method = "ns"
  
  #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
  Rate.Delta = 1
  
  # The first step is to read in the Bond Detail
  bond.id <- readRDS(paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""))
  #Call the desired curve from rates data folder
  trade.date = as.Date(trade.date, "%m-%d-%Y")
  rates.data <- readRDS(paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""))
  
  #The first step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)
  
  #The second step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
  #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
  BondCashFlow <- BondCashFlows(bond.id = bond.id, principal = principal, settlement.date = settlement.date, price = price)
  
  #The third step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  BondTermStructure <- BondTermStructure(bond.id = BondCashFlow, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
                                         principal = principal, price = price, cashflow = BondCashFlow)
  
  new("BondAnalytics", bond.id, BondCashFlow, BondTermStructure, TermStructure)
  
}




#---------------------------------------
# Bond Lab Initialize Set Generics
#---------------------------------------

#-------------------------------
#Bond Lab Set Methods for Helpers
#-------------------------------
