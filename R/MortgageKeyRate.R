# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

setGeneric("MortgageTermStructure", function(bond.id = "character", 
                                             original.bal = numeric(), 
                                             Rate.Delta = numeric(), 
                                             TermStructure = "character", 
                                             settlement.date = "character", 
                                             principal = numeric(), 
                                             price = numeric(), 
                                             cashflow = "character")
              {standardGeneric("MortgageTermStructure")})

setMethod("initialize",
         signature("MortgageTermStructure"),
         function(.Object,
                  SpotSpread = "numeric",   
                  EffDuration = "numeric",
                  EffConvexity = "numeric",
                  KeyRateTenor = "numeric",
                  KeyRateDuration = "numeric",
                  KeyRateConvexity = "numeric"){
           .Object@SpotSpread = SpotSpread
           .Object@EffDuration = EffDuration
           .Object@EffConvexity = EffConvexity
           .Object@KeyRateTenor = KeyRateTenor
           .Object@KeyRateDuration = KeyRateDuration
           .Object@KeyRateConvexity = KeyRateConvexity
           
           return(.Object)
           callNextMethod(.Object,...)
         })

# --------
# Mortgage Key Rate Duration Calculation
# ---------  
MtgTermStructure <- function(bond.id = "character", 
                             original.bal = numeric(), 
                             Rate.Delta = numeric(), 
                             TermStructure = "character", 
                             settlement.date = "character", 
                             principal = numeric(), 
                             price = numeric(), 
                             cashflow = "character"){
  
  # Depends on objects:
  # bond.id
  # Term Strucuture
  # Mortgage or Bond CashFlows    
  

  # Open connections to prepayment model tune parameters and  mortgage rate model

  
  # Open connection to the prepayment model tuning library
  ModelTune <- ModelTune(bond.id = bond.id)
  Burnout = bond.id@Burnout
  
  # Open connection to the Mortgage Model function
  MortgageRate <- MtgRate()
  
  #Burnout = bond.id@Burnout
  
  #Call the bond frequency to adjust the spot spread to the payment frequency of the bond
  frequency = bond.id@Frequency
  maturity = bond.id@Maturity
  accrued = cashflow@Accrued
  
  #Class name variable.  This will set the class name for the new class to be initilized
  ClassName <- if(bond.id@BondType != "MBS") {as.character("BondTermStructure")} else {as.character("MortgageTermStructure")}
  
  # create price for mortgage cash flow call in key rate
  price.mtg.cashflow <- price
 
  
  #Error Trap the user's price input
  if(price <= 1) {price = price} else {price = price/100}
  if(price <=0) stop("No valid bond price")
  
  # calcuate proceeds the order of operation is important
  # calculate price basis and then proceeds based on the correct price basis
  proceeds = (principal * price) + accrued 
  #========== Set the functions that will be used ==========
  # These functions are set as internal functions to key rates
  # this insures that stored values will not be wrongly be passed to the function
  # internal functions used to compute key rate duration and convexity
  
  EffectiveMeasures <- function(rate.delta , 
                                cashflow, 
                                cashflow.up, 
                                cashflow.dwn, 
                                discount.rates, 
                                discount.rates.up, 
                                discount.rates.dwn, 
                                t.period, 
                                proceeds, 
                                type){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow.up)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow.dwn)
    
    switch(type,
           duration = (Price.UP - Price.DWN)/(2*proceeds*rate.delta),
           convexity =  (Price.UP + Price.DWN - (2*proceeds))/(2 * proceeds * rate.delta^2)
    )
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
  
  # ==================== Key Rate Alogrithm Starts Here ==============================
  
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
  
  # -------------------------------------------------------------------------------------
  # Initialize the cash flow array for discounting and spot rate caclulations
  # this will be populated from class BondCashFlows
  # !!!!!!!!!!!!!!!!!DIM CASHFLOW ARRAY TO SIZE OF CASHFLOW!!!!!!!!!!!!!!!!!!!!!!
  # This will be the first pass at the cash flows before shocking the curve
  # --------------------------------------------------------------------------------------
  
  # Dimension the cashflow array for key rate discounting
  CashFlowArray <- array(data = NA, c(360,4), 
                         dimnames = list(seq(1:360), c("period", "cashflow_nc", "cashflow_dwn", "cashflow_up")))
  
  # Initialze the spot rate array for key rate duration calculations
  # The spot rate must be passed from Term Strucuture object to Bond Term Strucuture
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
  
  #----------- Step Two: Initialize loop and set the cashflows in the array ---------------------
  # This loops through the time period and set the cashflows into the proper array location for
  # discounts by indexing the cashflows to the array.  The indexing is conditional on the integer of the first period less than or equal to 1
  
  # This code populates the cashflows from the cashflow array object which is BondTermStructure via cashflows input
  
  if(as.integer(cashflow@TimePeriod[1] *12) != 1) CashFlowArray[as.integer(cashflow@TimePeriod * 12) + 1,2] = cashflow@TotalCashFlow
  if(as.integer(cashflow@TimePeriod[1] * 12) == 1) CashFlowArray[as.integer(cashflow@TimePeriod * 12),2] = cashflow@TotalCashFlow
  
  
  # This code solves for the spread to spot curve to equal price
  spot.spread <- uniroot(Spot.Spread, interval = c(-.75, .75), tol = .0000000001, CashFlowArray[,2],
                         discount.rates = Key.Rate.Table[,3], t.period = Key.Rate.Table[,2] , proceeds)$root
  
  #convert the spot spread to the frequency of the bond
  #spot.spread = (((1+spot.spread)^(1/frequency))-1) * frequency
  
  #------------- Step three add the spot spread to the spot curve 
  # This yields the discount rates that are need for the key rate duration calculation
  # at a minimum the cash flow array should be 360 months
  
  for(i in 1:360){
    Key.Rate.Table[i,4] = Key.Rate.Table[i,3] + spot.spread                                  
  }
  
  #========= Populate KRIndex Table =========================
  # The key rate index table will serve as the control table for the looping
  # using this table allows for incremental looping of discontinous segments of the
  # spot rate curve and is proprietary to bondlab
  # This table was dimensioned on lines 646 - 648
  
  # Step 1 populate Period (n)
  KRIndex[1:KRCount,1] <- round(as.numeric(KR) * 12,0)
  
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
      #This allocates the up and down Key.Rate.Table index.  col 5 is KR down and 6 is KR up
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
      a = KRIndex[w-1,1] + Rate.Delta 
      b = KRIndex[w+1,1] - Rate.Delta
      for(h in a : b){
        Key.Rate.Table[h,(z+4)] <- approx(KRx,KRy, Key.Rate.Table[h,2])$y
      } # Loop through Key Rate Table and interpolation
    } # Inner Loop to set interpolation points from KRIndex
    
    # This line sets the end points for discounting when the 30-year is last point on the curve
    # It is possible to set the endpoints longer using row 12 of the KRIndex
    
    if (KRIndex[w,2] == 30) {(Key.Rate.Table[x,5] = KRIndex[12,5]) & (Key.Rate.Table[x,6] = KRIndex[12,6])}
    
    #------------------------------------------------------------------------
    # Derive Key Rate Up and Key Rate Down Cash Flows
    # For MBS Cashflows two term strucutre objects are created the up and down term strucutres
    # are used to drive the key rate scenario foward rates and the up and down MBS cashflows
    # This suggests a separate Bond Term Structure for each; Bond Non-Callable, Bond Callable, and  Mortgage Term Strucutre
    # I have the original Bond Non-Callable Term Structure Code
    # Each can be incorporated to a final BondLab call using a switch command
    #-----------------------------------------------------------------------
    
    # Initialize the TermStructure Up and Down objects 
    # Use the term strucutre object 
    
    Key.Rate.TS.Dwn <- TermStructure
    Key.Rate.TS.Up <- TermStructure
    
    Key.Rate.TS.Dwn@spotrate <- c((Key.Rate.Table[,5]-spot.spread) * 100, 
                                  ((TermStructure@spotrate[361:492])) + (spot.spread * 0)
    )
    
    Key.Rate.TS.Dwn@TwoYearFwd <- (((1 + Key.Rate.TS.Dwn@spotrate[seq(from = 25, to = 385, by = 1)]) ^ (Key.Rate.TS.Dwn@period[seq(from = 25, to = 385, by = 1)]/12) /
                                      (1 + Key.Rate.TS.Dwn@spotrate[seq(from = 1, to = 361, by = 1)]) ^ (Key.Rate.TS.Dwn@period[seq(from = 1, to = 361, by = 1)]/12))^(1/2))-1
    
    Key.Rate.TS.Dwn@TenYearFwd <- (((1 + Key.Rate.TS.Dwn@spotrate[seq(from = 121, to = 481, by = 1)]) ^ (Key.Rate.TS.Dwn@period[seq(from = 121, to = 481, by = 1)]/12) /
                                      (1 + Key.Rate.TS.Dwn@spotrate[seq(from = 1, to = 361, by = 1)]) ^ (Key.Rate.TS.Dwn@period[seq(from = 1, to = 361, by = 1)]/12))^(1/10))-1
    
    
    Key.Rate.TS.Up@spotrate <- c((Key.Rate.Table[,6]-spot.spread) * 100, 
                                 ((TermStructure@spotrate[361:492])) + (spot.spread * 0)
    )
    
    Key.Rate.TS.Up@TwoYearFwd <- (((1 + Key.Rate.TS.Up@spotrate[seq(from = 25, to = 385, by = 1)]) ^ (Key.Rate.TS.Up@period[seq(from = 25, to = 385, by = 1)]/12) /
                                     (1 + Key.Rate.TS.Up@spotrate[seq(from = 1, to = 361, by = 1)]) ^ (Key.Rate.TS.Up@period[seq(from = 1, to = 361, by = 1)]/12))^(1/2))-1
    
    Key.Rate.TS.Up@TenYearFwd <- (((1 + Key.Rate.TS.Up@spotrate[seq(from = 121, to = 481, by = 1)]) ^ (Key.Rate.TS.Up@period[seq(from = 121, to = 481, by = 1)]/12) /
                                     (1 + Key.Rate.TS.Up@spotrate[seq(from = 1, to = 361, by = 1)]) ^ (Key.Rate.TS.Up@period[seq(from = 1, to = 361, by = 1)]/12))^(1/10))-1
    
    
    # Run the prepayment model to derive the SMM vector given each Key Rate shift
    # =======================================================================   
    # Key Rate Shift Down Prepayment Model and CashFlows
    # ======================================================================
    Prepayment.Dwn <- PrepaymentAssumption(bond.id = bond.id, 
                                           MortgageRate = MortgageRate, 
                                           TermStructure = Key.Rate.TS.Dwn, 
                                           PrepaymentAssumption = "MODEL", 
                                           ModelTune = ModelTune, 
                                           Burnout = Burnout) 
    
    # Mortgage Cashflows call here requires that price as whole number passed
    MortgageCashFlows.Dwn <- MortgageCashFlow(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date,
                                               price = price.mtg.cashflow, PrepaymentAssumption = Prepayment.Dwn)
    
    # Assign CashFlows into the cash flow array.  This has to be done in a loop
    for(cfd in 1:360){
      if(cfd > as.numeric(length(MortgageCashFlows.Dwn@TotalCashFlow))) {CashFlowArray[cfd,3] = 0} else 
      {CashFlowArray[cfd,3] = MortgageCashFlows.Dwn@TotalCashFlow[cfd]}
    }
    # =======================================================================   
    # Key Rate Shift Up Prepayment Model and CashFlows
    # ======================================================================
    Prepayment.Up <- PrepaymentAssumption(bond.id = bond.id, 
                                          MortgageRate = MortgageRate, 
                                          TermStructure = Key.Rate.TS.Up, 
                                          PrepaymentAssumption = "MODEL", 
                                          ModelTune = ModelTune, 
                                          Burnout = Burnout)
    
    # Mortgage Cashflows call here requires that price as whole number passed
    MortgageCashFlows.Up <- MortgageCashFlow(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date,
                                              price = price.mtg.cashflow, PrepaymentAssumption = Prepayment.Up)
    
    
    # Assign CashFlows into the cash flow array. This has to be done in a loop
    for(cfu in 1:360){
      if(cfu > as.numeric(length(MortgageCashFlows.Up@TotalCashFlow))) {CashFlowArray[cfu,4] = 0} else 
      {CashFlowArray[cfu,4] = MortgageCashFlows.Up@TotalCashFlow[cfu]}
    }
    
    #============================== Calculate Key Rate Duration ============================================
    KR.Duration[w-1,2] <- -EffectiveMeasures(
      rate.delta = Rate.Delta/100, 
      cashflow = CashFlowArray[,2],
      cashflow.dwn = CashFlowArray[,3],
      cashflow.up = CashFlowArray[,4],
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
      cashflow.dwn = CashFlowArray[,3],
      cashflow.up = CashFlowArray[,4],
      discount.rates = Key.Rate.Table[,4], 
      discount.rates.up = Key.Rate.Table[,6],
      discount.rates.dwn = Key.Rate.Table[,5],
      t.period = Key.Rate.Table[,2],
      type = "convexity",
      proceeds = proceeds
    ) 
  } # Outer Loop around KRIndex
  new(as.character(ClassName),
      SpotSpread = spot.spread * 100,
      EffDuration = sum(KR.Duration[,2]),
      EffConvexity = sum(KR.Duration[,3]),
      KeyRateTenor = KR.Duration[,1],
      KeyRateDuration = KR.Duration[,2],
      KeyRateConvexity = KR.Duration[,3]
      
  )
} # End the function


