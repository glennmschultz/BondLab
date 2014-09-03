  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3
  # Copyright (C) 2014  Glenn M Schultz, CFA
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 
  

  #------------------------------------------------------
  #Mortgage cash flow function.  This function calculates the cash flow of a mortgage pass through security
  #-----------------------------------------------------
  MortgageCashFlows <- function(bond.id = "character", original.bal = numeric(), settlement.date = "character", 
                              price = numeric(), PrepaymentAssumption = "character"){
   
  #This function error traps mortgage bond inputs
  ErrorTrap(bond.id = bond.id, principal = original.balance, settlement.date = settlement.date, price = price)
  
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  delay = bond.id@PaymentDelay
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  
  #Mortgage specific inputs
  note.rate = bond.id@GWac
  FirstPmtDate = bond.id@FirstPmtDate
  FinalPmtDate = bond.id@FinalPmtDate
  servicing.fee = bond.id@Servicing
  pmi = bond.id@PMI
  g.fee = bond.id@Gfee
  
  #error trap function needs to be upated to reflect expansion of the bond class
  #needs servicing, pmi and gfee error trapping
  
  #  Validate the price and coupon passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(price <= 1) {price = price} else {price = price/100}
  if(coupon > 1) {coupon = coupon/100} else {coupon = coupon}
  if(note.rate > 1) {note.rate = note.rate/100} else {note.rate = note.rate}
  
  #calcualte beginning balance (principal) from the MBS pool factor
  factor = bond.id@MBSFactor
  principal = original.bal * factor
  
  #Calculate the number of cashflows that will be paid from settlement date to the last pmt date
  #step 1 calculate the years to maturity
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
  
  #step5 initialize the prepayment model assumption class
  
  col.names <- c("Period", "Date", "Time", "Begin Bal", "Monthly Pmt", "Scheduled Int", "Scheduled Prin", "Prepaid Prin", 
                 "Ending Bal", "Sevicing", "PMI", "GFee", "Pass Through Interest", "Investor CashFlow", "Present Value Factor", "Present Value", 
                 "Duration", "Convexity Time", "CashFlow Convexity", "Convexity")
    
  MBS.CF.Table <- array(data = NA, c(num.periods, 20), dimnames = list(seq(c(1:num.periods)),col.names))  
  for(x in 1:num.periods){
    MBS.CF.Table[x,1] = x
    MBS.CF.Table[x,2] = pmtdate[x] + delay
    MBS.CF.Table[x,3] = time.period[x]
    if (MBS.CF.Table[x,1] == 1) {MBS.CF.Table[x,4] = principal} else {MBS.CF.Table[x,4] = MBS.CF.Table[x-1,9]}
    MBS.CF.Table[x,5] = Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x,4], note.rate = note.rate, 
                                                 term.mos = (num.periods - MBS.CF.Table[x,1] + 1))
    
    MBS.CF.Table[x,6] = MBS.CF.Table[x,4] * (note.rate/12)
    MBS.CF.Table[x,7] = Sched.Prin(balance = MBS.CF.Table[x,4], note.rate = note.rate, 
                                   term.mos = (num.periods - MBS.CF.Table[x,1] + 1), period = 1)
        
    if(x != num.periods) {MBS.CF.Table[x,8] = PrepaymentAssumption@SMM[x] * (MBS.CF.Table[x,4] - MBS.CF.Table[x,7])} else                     
      {MBS.CF.Table[x,8] = 0}
    
    MBS.CF.Table[x,9] = MBS.CF.Table[x,4] - MBS.CF.Table[x,7] - MBS.CF.Table[x,8]
    MBS.CF.Table[x,10] = MBS.CF.Table[x,4] * (servicing.fee/1200)
    MBS.CF.Table[x,11] = MBS.CF.Table[x,4] * (pmi/1200)
    MBS.CF.Table[x,12] = MBS.CF.Table[x,4] * (g.fee/1200)
    MBS.CF.Table[x,13] = MBS.CF.Table[x,4] * (coupon/12)
    MBS.CF.Table[x,14] = MBS.CF.Table[x,13] + MBS.CF.Table[x,7] + MBS.CF.Table[x,8]
  }
  
  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, 
                        settlement.date = settlement.date, lastpmt.date = lastpmt.date, nextpmt.date = nextpmt.date)) * 360
  
  days.between.pmtdate = ((12/frequency)/12) * 360
  days.of.accrued = (days.between.pmtdate - days.to.nextpmt) 
  accrued.interest = (days.of.accrued/days.between.pmtdate) * MBS.CF.Table[1,13]
  
  #Step6 solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  #it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value 
  irr <- function(rate , time.period , cashflow , principal , price , accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  
  ytm = uniroot(irr, interval = c(lower = -1, upper = 1), tol =.0000000001, time.period = MBS.CF.Table[,3], 
                cashflow = MBS.CF.Table[,14], principal = principal, price = price, accrued.interest = accrued.interest)$root
  
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency
  
  #Step7 Present value of the cash flows Present Value Factors
  MBS.CF.Table[,15] = 1/((1+(Yield.To.Maturity/frequency))^(MBS.CF.Table[,3] * frequency))
  
  #Present Value of the cash flows
  MBS.CF.Table[,16] = MBS.CF.Table[,14] * MBS.CF.Table[,15]
  
  #Step8 Risk measures Duration Factors
  MBS.CF.Table[,17] = MBS.CF.Table[,3] * (MBS.CF.Table[,16]/((principal * price) + accrued.interest))
  
  #Convexity Factors
  MBS.CF.Table[,18] = MBS.CF.Table[,3] *(MBS.CF.Table[,3] + 1)
  MBS.CF.Table[,19] = (MBS.CF.Table[,14]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((MBS.CF.Table[,3] + 2) * frequency)))/ 
    ((principal * price) + accrued.interest)
  MBS.CF.Table[,20] = MBS.CF.Table[,18] * MBS.CF.Table[,19] 
  
  #Weighted Average Life
  WAL = sum((((MBS.CF.Table[,7]) + (MBS.CF.Table[,8])) * MBS.CF.Table[,3])/ sum((MBS.CF.Table[,7]) + (MBS.CF.Table[,8])))
  
  #Duration and Convexity
  Duration = apply(MBS.CF.Table, 2, sum)[17]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(MBS.CF.Table, 2, sum)[20] * .5
  
  #Create Class Mortgage Loan Cashflows
  new("MortgageCashFlows",
      bond.id,
      Price = price * 100,
      Accrued = accrued.interest,
      YieldToMaturity = Yield.To.Maturity,
      WAL = WAL,
      ModDuration = Modified.Duration,
      Convexity = Convexity,
      Period = MBS.CF.Table[,1],
      PmtDate = as.character(as.Date(MBS.CF.Table[,2], origin = "1970-01-01")),
      TimePeriod = MBS.CF.Table[,3],
      BeginningBal = MBS.CF.Table[,4],
      MonthlyPmt = MBS.CF.Table[,5],
      MonthlyInterest = MBS.CF.Table[,6],
      PassThroughInterest = MBS.CF.Table[,13],
      ScheduledPrin = MBS.CF.Table[,7],
      PrepaidPrin = MBS.CF.Table[,8],
      EndingBal = MBS.CF.Table[,9],
      ServicingIncome = MBS.CF.Table[,10],
      PMIPremium = MBS.CF.Table[,11],    
      GFeePremium = MBS.CF.Table[,12],
      TotalCashFlow = MBS.CF.Table[,14]
  )
  }  
 
 
  # --------
  # Mortgage Key Rate Duration Calculation
  # ---------  
  MtgTermStructure <- function(bond.id = "character", original.bal = numeric(), Rate.Delta = numeric(), TermStructure = "character", 
                                settlement.date = "character", principal = numeric(), price = numeric(), cashflow = "character"){

  # Depends on objects
  # bond.id
  # Term Strucuture
  # Mortgage or Bond CashFlows    
    
  # =============================================
  # Open connections to prepayment model tune parameters and  mortgage rate model
  # =============================================
        
  # Open connection to the prepayment model tuning library
    connBTS2 <- gzfile(description = paste("~/BondLab/PrepaymentModel/",bond.id@Model,".rds", sep = ""), open = "rb")
    ModelTune <- readRDS(connBTS2)
    Burnout = bond.id@Burnout
    
  # Open connection to the Mortgage Model function
    connBTS3 <- gzfile("~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
    MortgageRate <- readRDS(connBTS3)  
    Burnout = bond.id@Burnout
    
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
    
    EffectiveMeasures <- function(rate.delta , cashflow, cashflow.up, cashflow.dwn, 
                      discount.rates, discount.rates.up, discount.rates.dwn, t.period, proceeds, type){
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
      Prepayment.Dwn <- PrepaymentAssumption(bond.id = bond.id, MortgageRate = MortgageRate, TermStructure = Key.Rate.TS.Dwn, 
                                             PrepaymentAssumption = "MODEL", ModelTune = ModelTune, Burnout = Burnout, 
                                             begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
      
      # Mortgage Cashflows call here requires that price is converted back to unit of 100 otherwise uniroot fails
      # This is becasue price is converted in pass through analytics call - Ultimately both bond and mortgage will be called
      # via a single call to BondLab
      MortgageCashFlows.Dwn <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date,
                                                 price = price*100, PrepaymentAssumption = Prepayment.Dwn)
      
      # Assign CashFlows into the cash flow array.  This has to be done in a loop
      for(cfd in 1:360){
        if(cfd > as.numeric(length(MortgageCashFlows.Dwn@TotalCashFlow))) {CashFlowArray[cfd,3] = 0} else 
        {CashFlowArray[cfd,3] = MortgageCashFlows.Dwn@TotalCashFlow[cfd]}
      }
      # =======================================================================   
      # Key Rate Shift Up Prepayment Model and CashFlows
      # ======================================================================
      Prepayment.Up <- PrepaymentAssumption(bond.id = bond.id, MortgageRate = MortgageRate, TermStructure = Key.Rate.TS.Up, 
                                            PrepaymentAssumption = "MODEL", ModelTune = ModelTune, Burnout = Burnout, 
                                            begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
      
      # Mortgage Cashflows call here requires that price is converted back to unit of 100 otherwise uniroot fails
      # This is becasue price is converted in pass through analytics call - Ultimately both bond and mortgage will be called
      # via a single call to BondLab
      MortgageCashFlows.Up <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date,
                                                price = price*100, PrepaymentAssumption = Prepayment.Up)
      
      
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
  
  #--------------------------------------
  #Functions for the Cox, Ingersoll, Ross
  #Single Factor Model
  #--------------------------------------

  CIRBondPrice <- function(shortrate = vector(), T = numeric(), step = numeric(), kappa = numeric(), 
                           lambda = numeric(), sigma = numeric(), theta = numeric(), result = character){
    #Error trap the function
    
    if (missing(shortrate))
      stop("Need to specify shortrate.")
   
    # This error throws a warning in OAS
    #if (shortrate < 0 | shortrate > 1)
    #  stop("No valid interest.rate specified.")
    
    if (missing(T))
      stop("Need to specify maturity.")
    
    if (missing(step))
      stop("Need to specify step.")
    
    if (missing(kappa))
      stop("Need to specify kappa.")
    
    if (kappa < 0 | kappa > 1)
      stop("No valid kappa specified.")
    
    if (missing(lambda))
      stop("Need to specify lambda")
    
    if (lambda < 0 | lambda > 1)
      stop("No valid lambda specified.")
    
    if (missing(sigma))
      stop("Need to specify sigma")
    
    if (sigma < 0 | sigma > 1)
      stop("No valid sigma specified.")
    
    if (missing(theta))
      stop("Need to specify theta")
    
    if (theta < 0 | theta > 1)
      stop("No valid theta specified.")
    
    if(missing(result))
      result = "p"  
    
    #T is the maturity is the zero coupon bond. To price a coupon paying bond this is the maturity of the bond
    #step the time between each payment of a coupon paying bond
    
    #kappa is the rate of mean reversion  
    #lambda is the market risk premium (lambda must be negative)
    #sigma is interest rate variance - specificlly sigma^2
    #theta is the mean interest rate level
    #T is the vector of maturities of the yield curve each cash flow is treated as a zero coupon bond  
    #T = c(seq(from = step, to = T, by = step))
    
    T = if(step != 0) {c(seq(from = step, to = T, by = step))} else {T}
    
    #t is the start period this is the first step of the simulation as well as the step size
    #for example from t = 0 to the next period is 0 + step
    t = step
    
    #The below are the functions to determine the closed form solution to the CIR Model
    gamma = ((kappa + lambda)^2 + (2 * (sigma^2))) ^ (1/2)
    
    A.CIR = ((2 * gamma * exp(((kappa + lambda + gamma) * (T-t))/2)) 
             /
               ((gamma + lambda + kappa) * (exp(gamma * (T-t)) - 1) + (2 * gamma))) ^ ((2 * kappa * theta)/(sigma^2))
    
    B.CIR = ((2 * (exp(gamma * (T-t)) - 1))
             /
               ((gamma + lambda + kappa) * (exp(gamma * (T-t)) - 1) + (2 * gamma)))
    
    Price = A.CIR * exp((B.CIR * -1) * shortrate) 
    Yield = (shortrate * B.CIR  - log(A.CIR))/(T-t)
    Limit = (2* kappa* theta) /(gamma + kappa + lambda)
    
    CIRBondPrice = switch(result,
                          p = Price,
                          y = Yield,
                          l = Limit)  
    
  }
  
  CIRSim <- function(shortrate = numeric(), kappa = numeric(), theta = numeric(), 
                      T = numeric(), step = numeric(), sigma = numeric(), N = numeric()){
    
    #kappa is the rate of mean reversion
    #theta is the long term value of the short rate
    #T is the horizon
    #step is the time between each payment of coupon paying bond
    #N is the number of simulations
    
    #Error Trap Model Parameters
    if(2*kappa*theta < sigma^2) 
      stop("Invaild parameterization origin is inaccessible")
    
    dt <- step
    nrow <-  T/dt
    

    deltarate <- function(kappa = numeric(), theta = numeric(), dt = numeric(), sigma = numeric()){
    #Populate the first element of each path with the short rate
    #Euler discretization of the CIR model.  The discretization causes negative interest rates when 
    #when the short term rate approaches the origin.  To solve this problem take the absolute value of square root process  
    (kappa * (theta - simulation[i-1,j]) * dt) + (sigma * sqrt(abs(simulation[i-1,j])) * rnorm(1,0,1))}  
    
    #Matrix to hold the short rate paths - I can dimnames here rather than colnames same as mortgage oas (rename N to paths?)
    simulation = array(data = 0, c((nrow + 1), N))
          simulation[1,] = shortrate
    
    for(j in 1:N){
      for(i in 2:(nrow + 1)){        
        simulation[i,j] <- simulation[i-1, j] + deltarate(kappa = kappa, theta = theta, dt = dt, sigma = sigma)    
      }
    }
    
    colnames(simulation) <- c(rep((paste("path", seq(1:N)))))
    return(simulation)
  }
  
  #---------------------------------
  # Calibrate CIR to market
  #________________________________
  CalibrateCIR <- function(trade.date = character){
    
    #Call the desired curve from rates data folder
    CalCIR1 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
    rates.data <- readRDS(CalCIR1)
    
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
    
    #data$PRICE <- rep(100, ColCount -1)
    data$PRICE <- ifelse(Mat.Years < 1, (1 + (Coupon.Rate/100))^(Mat.Years * -1) * 100, 100)
    
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
    
    #set term strucutre input (TSInput) to class couponbonds
    class(TSInput) <- "couponbonds"
    CashFlow <- TSInput[[1]]
    CIR.CF.Matrix <- create_cashflows_matrix(TSInput[[1]], include_price = TRUE)
    CIR.Mat.Matrix <- create_maturities_matrix(TSInput[[1]], include_price = TRUE )
    
    #Objective function
    CIRTune <- function(param = numeric(), shortrate = numeric(), sigma = .015, cfmatrix = matrix(), matmatrix = matrix()){
      kappa =  param[1]
      lambda = param[2]
      theta =  param[3]
      
      Disc <- CIRBondPrice(kappa = kappa, lambda = lambda, theta = theta, shortrate = shortrate, T= matmatrix,  step = 0, sigma = sigma)    
      CIRTune <- sqrt((sum(colSums((cfmatrix * Disc))^2))/ncol(matmatrix))
      return(CIRTune)
    }
    
    # Fit the model to the market   
    fit <- optimx(par = c(.1, .003, .03), 
                  fn = CIRTune, 
                  method = "L-BFGS-B",
                  lower = rep(.001, 3),
                  upper = rep(1, 3),
                  shortrate = .0016,
                  sigma = .015,
                  cfmatrix = CIR.CF.Matrix, 
                  matmatrix = CIR.Mat.Matrix)  
    close(CalCIR1)
     return(fit)
  }
  
  #-----------------------------------
  # Mortgage OAS Function
  #___________________________________
  
  Mortgage.OAS <- function(bond.id = "character", trade.date = "character", settlement.date = "character", original.bal = numeric(),
                          price = numeric(), short.rate = numeric(), sigma = numeric(), paths = numeric(), TermStructure = "character"){

    #The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
    conn1 <-  gzfile(description = paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""), open = "rb")
    bond.id = readRDS(conn1)
    
    # Establish connection to mortgage rate model
    conn2 <- gzfile(description = "~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
    MortgageRate <- readRDS(conn2)
    
    # Establish connection to prepayment model tuning parameter
    conn3 <- gzfile(description = paste("~/BondLab/PrepaymentModel/", as.character(bond.id@Model), ".rds", sep =""), open = "rb")        
    ModelTune <- readRDS(conn3)
    
    #Call the desired curve from rates data folder
    conn4 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
    rates.data <- readRDS(conn4)
               
    issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
    start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
    end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
    lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
    nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
    coupon = bond.id@Coupon
    frequency = bond.id@Frequency
    delay = bond.id@PaymentDelay
    factor = bond.id@MBSFactor
    settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
    
    #The spot spread function is used to solve for the spread to the spot curve to normalize discounting
    #This function is encapasulated in term structure
    
    Spot.Spread <- function(spread = numeric(), cashflow = vector(), discount.rates = vector(), 
                            t.period = vector(), proceeds = numeric()){
      Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
      return(proceeds - Present.Value)
    }
          
    #First, calibrate the interest rate model to market swap rates and prices
    #Set trade date and call the CalibrateCIR Model
    #trade.date = as.Date(trade.date, "%m-%d-%Y")
    
    Market.Fit <- CalibrateCIR(trade.date = trade.date)
    kappa  = Market.Fit$p1
    lambda = Market.Fit$p2
    theta  = Market.Fit$p3
     
    
    #Calculate the number of cashflows that will be paid from settlement date to the last pmt date (used end date as next pmdt date for this)
    ncashflows = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                     lastpmt.date = lastpmt.date, nextpmt.date = end.date) 
    
    #Build a vector of dates for the payment schedule
    #first get the pmtdate interval
    pmtdate.interval = months.in.year/frequency
      
    #Compute the payment dates
    pmtdate = as.Date(c(if(settlement.date == issue.date) {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} 
                        else {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y") + delay
    
    
    #Build the time period vector (n) for discounting the cashflows nextpmt date is vector of payment dates to n for each period
    time.period = BondBasisConversion(issue.date = issue.date, start.date = start.date, end.date = end.date, settlement.date = settlement.date,
                                      lastpmt.date = lastpmt.date, nextpmt.date = pmtdate)

    #step4 Count the number of cashflows 
    #num.periods is the total number of cashflows to be received
    #num.period is the period in which the cashflow is received
    num.periods = length(time.period)
    num.period = seq(1:num.periods)
        
    #==== Compute Option Adjusted Spread ==========================================
    #For simulation pass T = mortgage term if the number of paths = 1 then volatility = 0 
    Simulation <- CIRSim(shortrate = short.rate, kappa = kappa, theta = theta, T = ((num.periods-1) / months.in.year), step = (1/months.in.year), sigma = sigma, N = paths)
      
    #number of rows in the simulation will size the arrays
    num.sim <- nrow(Simulation)
    
    #Dim arrays for the calculation
    cube.names <- c("Period", "Date", "Time", "SpotRate", "DiscRate", "TwoYear", "TenYear")    
    sim.cube <- array(data = NA, c(num.sim, 7), dimnames = list(seq(c(1:num.sim)),cube.names))

    #Populate the simulation cube  
    sim.cube[,1] <- num.period
    sim.cube[,2] <- pmtdate
    sim.cube[,3] <- time.period
    
    #Dimension the arrays that will be needed
    oas.names <- c("OAS", "WAL", "ModDur", "YTM", "Price")
    #OAS out holds OAS solutions to individual trajectory calcualtions solving for the spread to price
    OAS.Out <- array(data = NA, c(paths,5), dimnames = list(seq(c(1:paths)),oas.names))
    
    OAS.CashFlow <- array(data = NA, c(num.sim,paths))
    OAS.DiscMatrix <- array(data = NA, c(num.sim, paths))
  
      
    for(j in 1:paths){
    
    #calculate spot rate for discounting  ([,5] multiplied by 100 for TermStructure - tried it did not work)
    #sim cube 5 ifelse synchs the CIR output to that of term strucutred for MBS cashflow analysis this needs to be fixed
    #rates should be passed through in a common scales regardless of interest rate model  
    sim.cube[,4] <- cumprod(1 + Simulation[,j])
    #sim.cube 5 is the discount rate to value cash flows
    sim.cube[,5] <- (((sim.cube[,4] ^ (1/ sim.cube[,3]))^(1/months.in.year))-1)
    
    sim.cube[,6] <- as.vector(CIRBondPrice(shortrate = as.numeric(Simulation[, j]), 
                    kappa = kappa, lambda = lambda, theta = theta, sigma = sigma, T = 2, step = 0, result = "y") * 100)
    
    sim.cube[,7] <- as.vector(CIRBondPrice(shortrate = Simulation[, j], 
                    kappa = kappa, lambda = lambda, theta = theta, sigma = sigma, T = 10, step = 0, result = "y") * 100)
      

      #Initialize OAS Term Structure object.  This object is passed to prepayment assumption
      #Allows the prepayment model to work in the Option Adjusted Spread function replacing Term Structure
      #When sigma is zero the simulated spot rates are compounded forward rates and the two and ten year
      #rates are calcualted from the calculated spot rate rate curve
    
      if (TermStructure != "TRUE")
      OAS.Term.Structure <- new("TermStructure",
                                tradedate = as.character(trade.date),
                                period = as.numeric(sim.cube[,3]),
                                date = unname(as.character(as.Date(sim.cube[,2], origin = "1970-01-01"))),
                                spotrate = as.numeric(sim.cube[,5]),
                                forwardrate = as.numeric(Simulation[,j]),
                                TwoYearFwd = as.numeric(sim.cube[,6]),
                                TenYearFwd = as.numeric(sim.cube[,7]))
    
      else
      OAS.Term.Structure <- new("TermStructure",
                                tradedate = as.character(as.Date(trade.date, "%m-%d-%Y")),
                                period = as.numeric(sim.cube[,1]),
                                date = unname(as.character(as.Date(sim.cube[,2], origin = "1970-01-01"))),
                                spotrate = as.numeric(sim.cube[,5]) * 100,
                                forwardrate = as.numeric(Simulation[,j] * 100),
                                TwoYearFwd = as.numeric(sim.cube[,6]),
                                TenYearFwd = as.numeric(sim.cube[,7]))
                
      Prepayment <- PrepaymentAssumption(bond.id = bond.id, TermStructure = OAS.Term.Structure, MortgageRate = MortgageRate, 
                           PrepaymentAssumption = "MODEL", ModelTune = ModelTune, Burnout = Burnout)
    
      MtgCashFlow <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                                     price = price, PrepaymentAssumption = Prepayment)
      
      OAS.CashFlow[,j] <- as.vector(MtgCashFlow@TotalCashFlow)
      OAS.DiscMatrix [,j] <- as.vector(sim.cube[,5])

        
    #error trapping of price is above currently line 1533 
    proceeds <- as.numeric((original.bal * factor * price/100) + MtgCashFlow@Accrued)
        
    #Solve for spread to spot curve to equal price (OAS Term Strucutre divided by 100 - tried did not work)
    OAS.Out[j,1] <- uniroot(Spot.Spread, interval = c(-1, 1), tol = .0000000001, cashflow = MtgCashFlow@TotalCashFlow,
                             discount.rates = OAS.Term.Structure@spotrate, t.period = OAS.Term.Structure@period , proceeds)$root
    OAS.Out[j,2] <- MtgCashFlow@WAL
    OAS.Out[j,3] <- MtgCashFlow@ModDuration
    OAS.Out[j,4] <- MtgCashFlow@YieldToMaturity
     
  } # end of the OAS j loop
  
    # Calculate OAS spread find the spread such that the average proceeds is equal to proceeds
      OAS <- function(spread = numeric(), DiscountMatrix = matrix(), CashFlowMatrix = matrix(), period = vector(), proceeds = numeric(), paths = numeric()) {
      OAS.Proceeds <- data.frame(((1/((1 + OAS.DiscMatrix[,] + spread)^ period)) * OAS.CashFlow[,]))
      OAS.Proceeds <- (colSums(OAS.Proceeds)/proceeds) * 100
      return(mean(OAS.Proceeds) - price)}
      
      OAS.Spread <- uniroot(OAS, interval = c(-1,1), tol = .000000001, DiscountMatrix = OAS.DiscMatrix, CashFlowMatrix = OAS.CashFlow,
                             period = OAS.Term.Structure@period, proceeds = proceeds, paths = paths)$root
  
    #Calculate OAS to price for price distribution
    OAS.Price <- function(spread = numeric(), DiscountMatrix = matrix(), CashFlowMatrix = matrix(), period = vector(), proceeds = numeric(), paths = numeric()) {
    OAS.Proceeds <- data.frame(((1/((1 + OAS.DiscMatrix[,] + spread)^ period)) * OAS.CashFlow[,]))
    OAS.Proceeds <- (colSums(OAS.Proceeds)/proceeds) * 100
    return(OAS.Proceeds)}
  
    Price.Dist <- OAS.Price(OAS.Spread, DiscountMatrix = OAS.DiscMatrix, CashFlowMatrix = OAS.CashFlow,
                            period = OAS.Term.Structure@period, proceeds = proceeds, paths = paths)
    OAS.Out[,5] <- Price.Dist 

    # Calculate static cash flow spread to the curve at zero volatility
    # Using the prepayment model this will always match the ZV spread indiciating the pricing benchmark is exact
    # In reality the spread to the curve will be based on the pricing speed used.
    # This is a good check but in reality the spread to the curve must be calculated in the PassThrough OAS and passed to 
    # ZeroVolatility Object
    if (paths == 1) {                                   
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ as.numeric(rates.data[2,2:12]), data = data.frame(rates.data))  
    SpreadtoCurve = ((MtgCashFlow@YieldToMaturity  * 100) - predict(InterpolateCurve, MtgCashFlow@WAL ))/100
    }
  
    if (TermStructure != "TRUE")      
  
    {new("MortgageOAS",
      OAS = OAS.Spread,
      ZVSpread = mean(OAS.Out[,1]),
      SpreadToCurve = 999,
      PathSpread = OAS.Out[,1],
      PathWAL = OAS.Out[,2],
      PathModDur = OAS.Out[,3],
      PathYTM =OAS.Out[,4],
      PriceDist = OAS.Out[,5]
    )}

    else OAS.Term.Structure

  }
  
  
  #----------------------------------
  #Prepayment Model Functions.  These functions are used to build the base prepayment model
  #----------------------------------
    
  Seasoning <- function(alpha = numeric(), beta = numeric(), theta = numeric(), LoanAge = numeric()){
  #----------------------------------
  # Seasoning function is a 3-parameter asymtote exponential function where
  # The three parameter asymptote is equivalent to the PPC ramp
  # a is the asymptote of the function
  # b is the intercept of the function
  # c is the point where the max CPR is achieved
    
    if (missing(alpha))
      stop("Need to specify alpha tuning parameter.")
    if (!is.numeric(alpha)  )
      stop("No numeric alpha specified.")
    
    if (missing(beta))
      stop("Need to specify beta tuning parameter.")
    if (!is.numeric(beta)  )
      stop("No numeric beta specified.")
    
    if (missing(theta))
      stop("Need to specify theta tuning parameter.")
    if (!is.numeric(theta)  )
      stop("No numeric theta specified.")
    
    if (missing(LoanAge))
      stop("Need to specify theta tuning parameter.")
    if (!is.numeric(LoanAge)  )
      stop("No numeric theta specified.")
    
    alpha - beta * exp(-theta * LoanAge)}
  
  #----------------------------------
  # Seasonality is modeled as a sin wave
  # a is the amplitude of the wave an set the maximum seasonal factor
  # Month is the calendar month (1..., 12) numeric
  # b is a location parameter shifts the peak values > 1 shift left values < 1 shift right
  Seasonality <- function( alpha = numeric(), Month = numeric(), theta= numeric()){
    
    if (missing(alpha))
      stop("Need to specify alpha tuning parameter.")
    if (!is.numeric(alpha)  )
      stop("No numeric alpha specified.")
    
    if (missing(Month))
      stop("Need to specify Month variable.")
    if (!is.numeric(Month)  )
      stop("No numeric alpha specified.")
    
    if (missing(theta))
      stop("Need to specify Month variable.")
    if (!is.numeric(theta)  )
      stop("No numeric alpha specified.")
    
    (1  + alpha *sin((pi/2 * (Month + theta - 3)) / 3 - 1))}
  
  #-------------------------------------
  # arctanget function with a location parameter
  Borrower.Incentive <- function(incentive = numeric(), theta1 = numeric(), theta2 = numeric(), beta = numeric(), location = numeric()) { 
    theta1 + theta2 * atan(incentive + pi * (beta * ((location - atan(incentive))/pi))) 
  }
  
  #-----------------------------------
  # Burnout is an exponentially decreasing function
  # a is the coefficient on the burnout varaible and b is the measure of burnout
  Burnout <- function(beta1 = numeric(), beta2= numeric(), MaxIncen = numeric(), LoanAge = numeric()){
    exp(beta1 * LoanAge +  beta2 * MaxIncen)
  }  
  
  # The Bond Lab base prepayment model
  #----------------------------------------------------------------------------------------------------
  Prepayment.Model <- function(ModelTune = "character", LoanAge = vector(), 
                               Month = vector(), incentive = vector(), Burnout.maxincen = numeric()){
    
      TurnoverRate        = ModelTune@TurnoverRate                       
      Seasoning.alpha     = ModelTune@Turnover.alpha
      Seasoning.beta      = ModelTune@Turnover.beta 
      Seasoning.theta     = ModelTune@Turnover.theta
      Seasonality.alpha   = ModelTune@Seasonality.alpha
      Seasonality.theta   = ModelTune@Seasonality.theta
      Fast.theta1         = ModelTune@Incentive.Fast.theta.1  
      Fast.theta2         = ModelTune@Incentive.Fast.theta.2 
      Fast.beta           = ModelTune@Incentive.Fast.beta 
      Fast.location       = ModelTune@Incentive.Fast.eta
      Slow.theta1         = ModelTune@Incentive.Slow.theta.1 
      Slow.theta2         = ModelTune@Incentive.Slow.theta.2 
      Slow.beta           = ModelTune@Incentive.Slow.beta 
      Slow.location       = ModelTune@Incentive.Slow.eta
      Burnout.beta1       = ModelTune@Burnout.beta.1 
      Burnout.beta2       = ModelTune@Burnout.beta.2
        
      Turnover.Rate <- 1-(1 - TurnoverRate)^(1/12)
    
      Turnover <- Turnover.Rate * 
      Seasoning(alpha = Seasoning.alpha, beta = Seasoning.beta, theta = Seasoning.theta, LoanAge = LoanAge) *
      Seasonality(alpha = Seasonality.alpha, Seasonality.theta, Month = Month)
    
    # Calculate the Borrower Refinance Response
    Fast <- Borrower.Incentive(incentive = incentive, theta1 = Fast.theta1, theta2 = Fast.theta2, beta = Fast.beta, location = Fast.location)
    Slow <- Borrower.Incentive(incentive = incentive, theta1 = Slow.theta1, theta2 = Slow.theta2, beta = Slow.beta, location = Slow.location)
    Burnout <-Burnout(beta1 = Burnout.beta1, beta2 = Burnout.beta2, MaxIncen = Burnout.maxincen, LoanAge = LoanAge)
    
    Refinance <- (Fast * Burnout) + (Slow * (1-Burnout))
    
    SMM = Refinance + Turnover    
    SMM <-pmax(0, SMM)    
  }
  
  # ---------  This function is the prepayment model and serves as a constructor for the prepayment model vector 
  # ---------  Prepayment Assumption
  PrepaymentAssumption <- function(bond.id = "character", TermStructure = "character", MortgageRate = "character", ModelTune = "character", 
                                   Burnout = numeric(), PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), 
                                   seasoning.period = numeric(), CPR = numeric()){
    
    #Mortgage Rate is the call the to MortgageRDS.rds in the Prepayment Model folder.  Prepayment Assumption does not open a connection
    #to the MortgageRate.rds it must be open by the function that is calling Prepayment Model
    
    
    #Check for a valid prepayment assumption
    if(!PrepaymentAssumption %in% c("MODEL", "CPR", "PPC")) stop("Not a Valid Prepayment Assumption")
    PrepayAssumption <- PrepaymentAssumption    
    
    #Error Trap the CPR assumption
    if(PrepaymentAssumption == "CPR") if(CPR >=1) {CPR = CPR/100} else {CPR = CPR}
    #PPC function has error trapping feature so there is no need to error trap for PPC
    
    
    NoteRate = bond.id@GWac
    sato = bond.id@SATO
    FirstPmtDate = as.Date(bond.id@FirstPrinPaymentDate, "%m-%d-%Y")
    LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
    FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
    NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
    
    col.names <- c("Period", "PmtDate", "LoanAge", "TwoYearFwd", "TenYearFwd", "MtgRateFwd", "SMM")
    
    Mtg.Term = as.integer(difftime(FinalPmtDate, FirstPmtDate, units = "days")/days.in.month) + 1
    Remain.Term = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/days.in.month) + 1
    Period = seq(from = 1, to = Remain.Term, by = 1)
    PmtDate = as.Date(NextPmtDate)  %m+% months(seq(from = 0, to = Remain.Term-1, by = 1)) 
    LoanAge = as.integer(difftime(as.Date(NextPmtDate)  %m+% months(seq(from = 1, to = Remain.Term, by = 1)), 
                                  FirstPmtDate, units = "days")/days.in.month) + 1
    
    NoteRate =  as.numeric(rep(NoteRate, length(LoanAge)))
    sato = as.numeric(rep(sato, length(LoanAge)))
    
    # Here the switch function is used to determine the mortgage function to propogate the forward mortgage rate
    # switch is used because there will be more than 2 or 3 rates in the future and if else will get messy
    # the switch function is encapsulated with prepayment assumption for now
    
    Mtg.Rate <- function(TermStructure = "character", type = "character", term = numeric()) {
      term = as.character(term)
      switch( type, 
              fixed = switch(term,
                             "30" = MortgageRate@yr30(two = TermStructure@TwoYearFwd[1:length(LoanAge)], ten = TermStructure@TenYearFwd[1:length(LoanAge)], sato = sato),
                             "15" = MortgageRate@yr15(two = TermStructure@TwoYearFwd[1:length(LoanAge)], ten = TermStructure@TenYearFwd[1:length(LoanAge)], sato = sato)
                             ), # end first nested switch statement
              arm = switch(term, 
                           "30" = 0 ) # end second nested switch statement
      ) # end of "n" the switch logic

    }
    
    Mtg.Rate <- Mtg.Rate(TermStructure = TermStructure, type = bond.id@AmortizationType, term = bond.id@AmortizationTerm)
    Mtg.Rate <- Mtg.Rate[1:length(LoanAge)] # This is why I need to make class classflow array
    
    Incentive =  as.numeric(NoteRate - Mtg.Rate)
    Burnout = bond.id@Burnout
    
      
    if(PrepaymentAssumption == "MODEL")
      {SMM = Prepayment.Model(ModelTune = ModelTune, LoanAge = LoanAge, Month = as.numeric(format(PmtDate, "%m")), 
                              incentive = Incentive, Burnout.maxincen = Burnout)} 
      else 
      {if(PrepaymentAssumption == "PPC") 
      {SMM = as.numeric(1-(1-PPC.Ramp(begin.cpr = begin.cpr, end.cpr = end.cpr, 
                                      season.period = seasoning.period, period = LoanAge))^(1/12))} 
      else
      {SMM = rep(1-(1-CPR)^(1/12), Remain.Term)}
      
      }
    
    new("PrepaymentAssumption",
        PrepayAssumption = as.character(PrepayAssumption),
        PPCStart = if(PrepaymentAssumption == "PPC") {begin.cpr} else {0},
        PPCEnd = if(PrepaymentAssumption == "PPC") {end.cpr} else {0},
        PPCSeasoning = if(PrepaymentAssumption == "PPC") {seasoning.period} else {0},
        NoteRate = as.numeric(NoteRate),
        FirstPmtDate = as.character(FirstPmtDate),
        LastPmtDate = as.character(LastPmtDate),
        FinalPmtDate = as.character(FinalPmtDate),
        Period = Period,
        PmtDate = as.character(PmtDate),
        LoanAge = as.numeric(LoanAge),
        MtgRateFwd = as.numeric(Mtg.Rate),
        Incentive = as.numeric(Incentive),
        SMM = as.numeric(SMM)
    )
        
  }
  
  # ---------------- This function is the dollar roll analysis ---------------------------
  # ---------------- Currently the function calcualtes the 1 month roll ------------------
  # ---------------- Upgrade the bond basis function for actual/actual day count ---------  
  DollarRoll <- function(bond.id = "character", price = numeric(), drop = numeric(), original.bal = numeric(), 
                         settlement.date = "character", fwd.settlement.date = "character", 
                         reinvestment.rate = numeric(), finance.rate = numeric(),
                         MortgageCashFlow = "character") {
    
    #need to error trap these inputs
    #reinvestment rate, drop
    
    
    #Error Trap the user's price input
    if(price <= 1) {price = price} else {price = price/100}
    if(price <= 0) stop("No valid bond price")
    
    #Error Trap the user's drop input
    #if(drop < 1/32) {drop = drop} else {drop = drop/100}
    
    #Here upgrade the bond basis function to include actual day count
    settlement.date = as.Date(settlement.date, "%m-%d-%Y")
    fwd.settlement.date = as.Date(fwd.settlement.date, "%m-%d-%Y")
    reinvestment.days = as.numeric(difftime(fwd.settlement.date, settlement.date, units = "days"))
    
    Factor = bond.id@MBSFactor
    CurrentBal = as.numeric(original.bal * Factor)
    BeginningMarketValue = original.bal * Factor * price
    
    IssueDate = as.Date(bond.id@IssueDate)
    DatedDate = as.Date(bond.id@DatedDate)
    Maturity = as.Date(bond.id@Maturity)
    Frequency = as.numeric(bond.id@Frequency)
    Coupon = as.numeric(bond.id@Coupon)
    
    FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
    LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
    NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
    RemainingTerm = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/days.in.month)
    FwdPrice = price - (drop/100)
    reinvestment.rate = reinvestment.rate 
    
    #Dollar Roll Hold versus Roll Analysis..
    #Dollar Roll Proceeds
    Accrued = MortgageCashFlow@Accrued
    TotalProceeds = BeginningMarketValue + Accrued
    ReinvestmentIncome = as.numeric(TotalProceeds * reinvestment.rate * (reinvestment.days/360))
    TotalRollProceeds = TotalProceeds + ReinvestmentIncome
    
    #Hold Proceeds - Calculate the value of holding the MBS
    ScheduledPrin = as.numeric(MortgageCashFlow@ScheduledPrin[1])
    PrepaidPrin = as.numeric(MortgageCashFlow@PrepaidPrin[1])
    PassThroughInterest = as.numeric(MortgageCashFlow@PassThroughInterest[1])
    RemainingBalance = as.numeric(MortgageCashFlow@EndingBal[1])
    
    #Forward settlement and payment dates to compute the value of holding versus rolling
    #Roll Settlement Dates forward by the roll months.  For example one month roll, two months
    #Default value is one month
    
    # Payment Dates work with the mortgage cashflow payment dates.
    # This should be a variable in the function allowing for two-, and three- month rolls
    FwdNextPmtDate = as.Date(MortgageCashFlow@PmtDate[2], "%Y-%m-%d")
    FwdLastPmtDate = LastPmtDate
    days.to.nextpmt = BondBasisConversion(issue.date = IssueDate, start.date = DatedDate, end.date = Maturity,
                                          settlement.date = fwd.settlement.date, lastpmt.date = FwdLastPmtDate, nextpmt.date = FwdNextPmtDate)
    days.to.nextpmt = days.to.nextpmt * 360
    days.between.pmtdate = ((12/Frequency)/12) * 360
    days.of.accrued = (days.to.nextpmt - days.between.pmtdate) 
    
    FutureValueofPmts = ScheduledPrin + PrepaidPrin + PassThroughInterest
    FuturePrincipalProceeds = RemainingBalance * FwdPrice 
    FwdAccrued = (days.of.accrued/days.between.pmtdate) * as.numeric(MortgageCashFlow@PassThroughInterest[2])
    FutureValueHold = FutureValueofPmts + FuturePrincipalProceeds + FwdAccrued
    
    #Compute the Roll Economics....
    #Hold or Roll Analysis and Economics of Trade  
    if (TotalRollProceeds > FutureValueHold) 
    HoldorRoll = as.character("Roll") else HoldorRoll = as.character("Hold")  
    Advantage = as.numeric(abs(TotalRollProceeds - FutureValueHold))
    
    #Compute the days between the roll settlment date and the payment date
    #This is to derive the discounted value of the carry between the settlement date and the payment date
    #The discount rate is applied to the Future Value of the payments and is the Discounted Value of the Carry
    MBSPmtDate = as.Date(MortgageCashFlow@PmtDate[1], "%Y-%m-%d")
    settlement.day.diff = as.integer(difftime(MBSPmtDate, fwd.settlement.date, units = "days"))
    DiscValueofCarry = FutureValueofPmts * (1/((1 +finance.rate) ^ (settlement.day.diff/360)))
    
    FutureValuePrinCarry = (RemainingBalance * price) + FwdAccrued + DiscValueofCarry
    FinanceCost = as.numeric(TotalProceeds * finance.rate * (reinvestment.days/361))
    TotalFutureValue = FutureValuePrinCarry - FinanceCost
    
    DropImpliedValue = ((TotalFutureValue - TotalProceeds)/RemainingBalance) * 32
    
    new("DollarRoll",
        # -- MBS price and settlment information
        SettlementDate = as.character(settlement.date),
        FwdSettlementDate = as.character(fwd.settlement.date),
        Price = price * 100,
        Drop = drop,
        FwdPrice = FwdPrice * 100,
        # -- MBS information
        GrossCoupon = bond.id@GWac,
        NetCoupon = bond.id@Coupon,
        OriginalTerm = bond.id@AmortizationTerm,
        RemainingTerm = RemainingTerm,
        OrigBalance = original.bal,
        CurrentBalance = CurrentBal,
        # -- Settlement information
        PrincipalProceeds = BeginningMarketValue,
        Accrued = Accrued,
        TotalProceeds = TotalProceeds,
        DaysInterest = reinvestment.days,
        ReinvestmentIncome = ReinvestmentIncome,
        # -- Determine MBS hold information
        ScheduledPrin = ScheduledPrin,
        PrepaidPrin = PrepaidPrin,
        PassThroughInterest = PassThroughInterest,
        FutureValueHold = FutureValueHold,
        RemainingBalance = RemainingBalance,
        FuturePrincipalProceeds = FuturePrincipalProceeds,
        FwdAccrued = FwdAccrued,
        # -- Compute Investor Financing Rates
        FinanceRate = finance.rate,
        ReinvestmentRate = reinvestment.rate,
        FutureValueRoll = TotalRollProceeds,
        FutureValuePrinCarry = FutureValuePrinCarry,
        DiscValueofCarry = DiscValueofCarry,
        # -- Compute Roll analysis
        HoldorRoll = HoldorRoll,
        Advantage = Advantage,
        TotalFutureValue = TotalFutureValue,
        DropImpliedValue = DropImpliedValue,
        MortgageCashFlow
    )
  }

  #------- Scenario Function --------
  # opens connection to scenario library
  #----------------------------------
  Mtg.Scenario <- function(bond.id ="character", trade.date = "character", settlement.date = "character", price = numeric(), proceeds = numeric(), 
                           spot.spread = numeric(), original.bal = numeric(), scenario.set = vector(), rates.data = "character", 
                           method = "character", PrepaymentAssumption = "character",..., ModelTune = "character", Burnout = numeric(),
                           begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()) { 
    
      if(missing(method)) method = "ns"
      
      ScenarioResult <- list()
      
      # First step open mortgage rate functions
      connS1 <- gzfile(description = "~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
      MortgageRate <- readRDS(connS1)
      
      #Call the desired curve from rates data folder
      trade.date = as.Date(trade.date, "%m-%d-%Y")
      
      connS2 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
      rates.data <- readRDS(connS2)
     
      #Call Prepayment Model
      connS3 <- gzfile(description = paste("~/BondLab/PrepaymentModel/",bond.id@Model,".rds", sep = ""), open = "rb")
      ModelTune <- readRDS(connS3)
      Burnout = bond.id@Burnout
           
      # ----------------------- Scenario Analysis --------------------------------  
      for(i in 1:length(scenario.set)){
      
      connS4 <- gzfile(description = paste("~/BondLab/Scenario/", as.character(scenario.set[i]), ".rds", sep =""), open = "rb")        
      Scenario <- readRDS(connS4) 
      
      # Third call the trade date rates data
      # Create variabel for the length of rates and rates data 
      rates <- rates.data
      
      # Fourth call the scenario 
      rates[1,2:length(rates)] <- Scenario@Formula(rates[1,1:length(rates)], Shiftbps = Scenario@Shiftbps)
      
      # Caclulate the term strucute
      TermStructure = TermStructure(rates.data = rates, method = method)
      
      # Run the prpeayment model
      Prepayment = PrepaymentAssumption(bond.id = bond.id, MortgageRate = MortgageRate, TermStructure = TermStructure, 
                                        PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                                        begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
      
      # Scenario Mortgage cash flow analysis 
      MortgageCashFlow = MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, 
                                           settlement.date = settlement.date, price = price, PrepaymentAssumption = Prepayment)
      
      # Calculate static cash flow spread to the curve                                      
      InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ as.numeric(rates.data[2,2:12]), data = data.frame(rates.data))  
      SpreadtoCurve = (MortgageCashFlow@YieldToMaturity * 100) - predict(InterpolateCurve, MortgageCashFlow@WAL )
            
      # The second step will be to calculate the scenario effective duration and convexity
      # I can do this with a different call that does not require BondTermStrucutre
      MortgageTermStructure = MtgTermStructure(bond.id = bond.id, original.bal = original.bal, Rate.Delta = 0.25, TermStructure = TermStructure,
                              settlement.date = settlement.date, principal = original.bal * MortgageCashFlow@MBSFactor, price = price, cashflow = MortgageCashFlow)
      
      # ---------------------------------------------------------------
      # Function to compute the horizon return 
      ReturnAnalysis <- function(Scenario = "character", settlement.date = "character", proceeds = numeric(), 
                                 MortgageTermStructure = "character", spot.spread = numeric(), HrzMonths = numeric(), 
                                 ReinvestmentRate = numeric()) {
      
      # Need to error trap the reinvestment rate     
      # Reinvestment of cash flow  
      
      ReceivedCF = Scenario@TotalCashFlow[1:HrzMonths]
      n.period = as.numeric(difftime(as.Date(Scenario@PmtDate[HrzMonths]), as.Date(Scenario@PmtDate[1:HrzMonths]), units = "days")/days.in.month)
      Reinvestment = ReceivedCF * (1 + (ReinvestmentRate/months.in.year)) ^ (n.period)
      Reinvestment = sum(Reinvestment)
      
      # Price the tail cash flow priced at horizon
      # Forward month indexes to the cashflow array
      FwdMonth = (HrzMonths + 1)
      FwdSettleDate = as.Date(settlement.date, "%m-%d-%Y")  %m+% months(HrzMonths)
      FwdCashFlowPmtDate = Scenario@PmtDate[FwdMonth:length(Scenario@PmtDate)]
      RemainingCF = Scenario@TotalCashFlow[FwdMonth:length(Scenario@TotalCashFlow)]
      n.period.fwd = as.numeric(difftime(as.Date(Scenario@PmtDate[FwdMonth:length(Scenario@PmtDate)]), as.Date(FwdSettleDate), units = "days")/days.in.month)
                  
      # The approach bases the investor return on the forward rate curve and forward rates.
      DiscountRate =  
      # Spot rates at horizon to length of cashflows
      (1+((TermStructure@spotrate[FwdMonth:length(Scenario@TotalCashFlow)] + spot.spread)/1200))  ^
      # Index time period to period less length of the horizon for discounting the forward price
      (-1 * n.period.fwd)
      
      DiscPV = sum(RemainingCF * DiscountRate)
      TotalHrzProceeds = Reinvestment + DiscPV
      
      Return = TotalHrzProceeds/proceeds 
      
      }
      
      HorizonReturn <- ReturnAnalysis(Scenario = MortgageCashFlow, MortgageTermStructure = MortgageTermStructure, proceeds = proceeds,
                              settlement.date = settlement.date, spot.spread = spot.spread, HrzMonths = 12, ReinvestmentRate = .0025)
      
      HorizonReturn = (HorizonReturn - 1) * 100
    
      
      Scenario <- new("Mtg.Scenario",  
                      Period = MortgageCashFlow@Period,
                      PmtDate = MortgageCashFlow@PmtDate,
                      TimePeriod = MortgageCashFlow@TimePeriod,
                      BeginningBal = MortgageCashFlow@BeginningBal,
                      PassThroughInterest = MortgageCashFlow@PassThroughInterest,
                      ScheduledPrin = MortgageCashFlow@ScheduledPrin,
                      PrepaidPrin = MortgageCashFlow@PrepaidPrin,
                      EndingBal = MortgageCashFlow@EndingBal,
                      TotalCashFlow = (MortgageCashFlow@PassThroughInterest + 
                      MortgageCashFlow@ScheduledPrin + 
                      MortgageCashFlow@PrepaidPrin),
                      spotrate = TermStructure@spotrate,
                      forwardrate = TermStructure@forwardrate,
                      SMM = Prepayment@SMM,
                      YieldToMaturity = MortgageCashFlow@YieldToMaturity,
                      WAL = MortgageCashFlow@WAL,
                      SpreadToInterCurve = SpreadtoCurve,
                      ModDuration = MortgageCashFlow@ModDuration,
                      Convexity = MortgageCashFlow@Convexity,
                      EffDuration = MortgageTermStructure@EffDuration,
                      EffConvexity =  MortgageTermStructure@EffConvexity,
                      KeyRateTenor =  MortgageTermStructure@KeyRateTenor,
                      KeyRateDuration =  MortgageTermStructure@KeyRateDuration,
                      KeyRateConvexity =  MortgageTermStructure@KeyRateConvexity,
                      HorizonReturn = HorizonReturn,
                      Scenario                      
      )
    
      ScenarioResult <- append(ScenarioResult, Scenario)
  
    } # end loop
      
  
    new("Mtg.ScenarioSet", 
        Scenario = ScenarioResult,
        MortgageCashFlow)    
    
  } # scenario end function
  
  
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


  # This function analyzes a standard pass through security and serves as the constructor function
  #--------------------------------------  
  PassThroughAnalytics <- function (bond.id = "character", MortgageRate = "character", original.bal = numeric(), price = numeric(), trade.date = "character", 
                                  settlement.date = "character", method = "character", scenario.set = vector(),
                                  PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()
                                 ) 
  {
      
  #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
  #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
  if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
  
  #Default method for TermStructure
  if(missing(method)) method = "ns"
  
  #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
  Rate.Delta = .25
  
  # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
  conn1 <-  gzfile(description = paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""), open = "rb")
  bond.id = readRDS(conn1)
  
  #Call the desired curve from rates data folder
  conn2 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
  rates.data <- readRDS(conn2)
  
  #Call Mortgage Rate Functions
  conn3 <- gzfile("~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
  MortgageRate <- readRDS(conn3)  
  Burnout = bond.id@Burnout
  
  #Call Prepayment Model Tuning Parameters
  conn4 <- gzfile(description = paste("~/BondLab/PrepaymentModel/", as.character(bond.id@Model), ".rds", sep =""), open = "rb")        
  ModelTune <- readRDS(conn4)
  
  #The second step is to call the desired coupon curve into memory 
  #This is done with the TermStructure function which creates the class TermStructure
  TermStructure <- TermStructure(rates.data = rates.data, method = method)

  #Third if mortgage security call the prepayment model
  PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, MortgageRate = MortgageRate, 
                          TermStructure = TermStructure, PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                          begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
    
  #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow.
  MortgageCashFlow <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                      price = price, PrepaymentAssumption = PrepaymentAssumption)
  
  #The fifth step is to calculate effective duration, convexity, and key rate durations and key rate convexities
  #This is done with the BondTermStructureFunction this creates the class BondTermStructure
  MortgageTermStructure <- MtgTermStructure(bond.id = MortgageCashFlow, original.bal = original.bal, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
                          settlement.date = settlement.date, principal = original.bal *  MortgageCashFlow@MBSFactor, price = price, cashflow = MortgageCashFlow)

  spread.to.spot = MortgageTermStructure@SpotSpread
  proceeds = ((price/100) * (original.bal * bond.id@MBSFactor)) + MortgageCashFlow@Accrued
  
  # The sixth step in scenario based analysis
  Scenario <- Mtg.Scenario(scenario.set = scenario.set, bond.id = bond.id, MortgageRate = MortgageRate, original.bal = original.bal, 
                           trade.date = trade.date, settlement.date = settlement.date, price = price, proceeds = proceeds, spot.spread = spread.to.spot, 
                                   PrepaymentAssumption = "MODEL")
  
  closeAllConnections()
  
  new("PassThroughAnalytics", bond.id, MortgageCashFlow, MortgageTermStructure, TermStructure, PrepaymentAssumption, Scenario)    
  }
  
  #---------------------------------
  #This function is for Pass Through OAS Analysis and serves constructor for OAS Analysis
  
  PassThroughOAS <- function(bond.id = "character", trade.date = "character", settlement.date = "character", original.bal = numeric(), 
                            price = numeric(), short.rate = numeric(), sigma = numeric(), paths = numeric(), 
                            PrepaymentAssumption = "character", ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()){
    
    #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
    #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
    if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
    
    
    #Rate Delta is set to 1 (100 basis points) for effective convexity calculation                          
    Rate.Delta = .25
    
    # The first step is to read in the Bond Detail, rates, and Prepayment Model Tuning Parameters
    conn1 <-  gzfile(description = paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""), open = "rb")
    bond.id = readRDS(conn1)
    
    #Call the desired curve from rates data folder
    conn2 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
    rates.data <- readRDS(conn2)
    
    #Call Mortgage Rate Functions
    conn3 <- gzfile("~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
    MortgageRate <- readRDS(conn3)  
    
    Burnout = bond.id@Burnout
    
    #Call Prepayment Model Tuning Parameters
    conn4 <- gzfile(description = paste("~/BondLab/PrepaymentModel/", as.character(bond.id@Model), ".rds", sep =""), open = "rb")        
    ModelTune <- readRDS(conn4)

    
    #Call OAS Term Strucuture to Pass to the Prepayment Model
    TermStructure <- Mortgage.OAS(bond.id = as.character(bond.id@ID), trade.date = trade.date, settlement.date = settlement.date, original.bal = original.bal, 
                                  price = price, short.rate = short.rate, sigma = sigma, paths = 1, TermStructure = "TRUE")
    
    #Third if mortgage security call the prepayment model
    PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, MortgageRate = MortgageRate, 
                                                 TermStructure = TermStructure, PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                                                 begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
    
    #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow.
    MortgageCashFlow <- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                                          price = price, PrepaymentAssumption = PrepaymentAssumption)
    
    #Calculate effective duration, convexity, and key rate durations and key rate convexities
    #This is done with the MtgTermStructureFunction this creates the class BondTermStructure
    #MortgageTermStructure <- MtgTermStructure(bond.id = MortgageCashFlow, original.bal = original.bal, Rate.Delta = Rate.Delta, TermStructure = TermStructure, 
    #settlement.date = settlement.date, principal = original.bal *  MortgageCashFlow@MBSFactor, price = price, cashflow = MortgageCashFlow)
           
    MortgageOAS  <- Mortgage.OAS(bond.id = as.character(bond.id@ID), trade.date = trade.date, settlement.date = settlement.date, original.bal = original.bal, 
                   price = price, short.rate = short.rate, sigma = sigma, paths = paths, TermStructure = "FALSE")
    
    #Calculate the spread to the curve and pass to Zero Volatility                                   
    InterpolateCurve <- loess(as.numeric(rates.data[1,2:12]) ~ as.numeric(rates.data[2,2:12]), data = data.frame(rates.data))  
    SpreadtoCurve = ((MortgageCashFlow@YieldToMaturity  * 100) - predict(InterpolateCurve, MortgageCashFlow@WAL ))/100
    
    MortgageOAS@SpreadToCurve <- SpreadtoCurve  
      
    new("PassThroughOAS", bond.id, MortgageCashFlow, MortgageOAS)
    
  }
   
  #----------------------------------
  #Agency Mortgage Dollar Roll
    
    DollarRollAnalytics <- function(bond.id = "character", original.bal= numeric(), price = numeric(), drop = numeric(), trade.date = "character", 
                           settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(),  
                           finance.rate = numeric(), method = "ns", PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric()) {
    
    #Error Trap Settlement Date and Trade Date order.  This is not done in the Error Trap Function because that function is 
    #to trap errors in bond information that is passed into the functions.  It is trapped here because this is the first use of trade date
    if(trade.date > settlement.date) stop ("Trade Date Must be less than settlement date")
    
    #Default method for TermStructure
    if(missing(method)) method = "ns"
    
    
    # Open bond.id connection
 
    connDR1 <- gzfile(description = paste("~/BondLab/BondData/",bond.id, ".rds", sep = ""), open = "rb")
    bond.id = readRDS(connDR1)
    
    # Open connection to rates data 
    trade.date = as.Date(trade.date, "%m-%d-%Y")
    connDR2 <- gzfile(description = paste("~/BondLab/RatesData/", trade.date, ".rds", sep = ""), open = "rb")
    rates.data = readRDS(connDR2)
    
    #Open Model Tune Connection
    connDR3 <- gzfile(description = paste("~/BondLab/PrepaymentModel/",as.character(bond.id@Model),".rds", sep = ""), open = "rb")
    ModelTune <- readRDS(connDR3) 
      
    # Open Mortgage Rate Connection
    connDR4<- gzfile(description = "~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
    MortgageRate <- readRDS(connDR4)
    
    Burnout = bond.id@Burnout
    #The second step is to call the desired coupon curve into memory 
    #This is done with the TermStructure function which creates the class TermStructure
    TermStructure <- TermStructure(rates.data = rates.data, method = method)
    
    # Third if mortgage security call the prepayment model
    PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id, TermStructure = TermStructure, MortgageRate = MortgageRate,
                                  PrepaymentAssumption = PrepaymentAssumption, ModelTune = ModelTune, Burnout = Burnout, 
                                  begin.cpr = begin.cpr, end.cpr = end.cpr, seasoning.period = seasoning.period, CPR = CPR)
    
    #The fourth step is to call the bond cusip details and calculate Bond Yield to Maturity, Duration, Convexity and CashFlow. 
    #The BondCashFlows function this creates the class BondCashFlows are held in class BondCashFlows
    MortgageCashFlow <<- MortgageCashFlows(bond.id = bond.id, original.bal = original.bal, settlement.date = settlement.date, 
                                          price = price, PrepaymentAssumption = PrepaymentAssumption)
    
  
    DollarRoll <- DollarRoll(bond.id = bond.id, price = price, drop = drop, original.bal = original.bal, 
                             settlement.date = settlement.date, fwd.settlement.date = fwd.settlement.date, 
                             reinvestment.rate = reinvestment.rate, finance.rate = finance.rate, MortgageCashFlow = MortgageCashFlow)
   return(DollarRoll)
  }
  


  #-----------------------
  # Classes these are the new classes used by by Bond Lab
  #------------------------

  setGeneric(
  name = "MortgageCashFlows",
  def = function(bond.id = "character", original.bal = numeric(), settlement.date = "character", 
                 price = numeric(), PrepaymentAssumption = "character")
  {standardGeneric("MortgageCashFlows")})

 setGeneric("TermStructure",
           function(rates.data = "character", method = "character")
           {standardGeneric("TermStructure")})

  setGeneric("PassThroughAnalytics",
           function (bond.id = "character", MortgageRate = "character", original.bal = numeric(), price = numeric(), trade.date = "character", 
                     settlement.date = "character", method = method, scenario.set = vector(), PrepaymentAssumption = "character",
                     ..., begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
             {standardGeneric("PassThroughAnalytics")})
  
  setGeneric("PrepaymentAssumption",
           function(bond.id = "character", TermStructure = "character", MortgageRate = "character", ModelTune = "character", Burnout = numeric(),
                    PrepaymentAssumption = "character",...,begin.cpr = numeric(), end.cpr = numeric(), 
                    seasoning.period = numeric(), CPR = numeric())
           {standardGeneric("PrepaymentAssumption")}
           )  

  setGeneric("RateofReturn",
           function(ReinvestmentRate = numeric(), ReceivedCF = "character", RemainingCF = "character",
                    SpotCurve = "character", FwdCurve = "character", HorizonSpread = numeric())
             {standardGenric("RateofReturn")})

  setGeneric("Seasoning",
           function (alpha = numeric(), beta = numeric (), theta = numeric(), LoanAge = numeric())
             {standardGeneric("Seasoning")})

  setGeneric("Borrower.Incentive",
           function(incentive = numeric(), theta1 = numeric(), theta2 = numeric(), beta = numeric(), location = numeric())
             {standardGeneric("Borrower.Incentive")})

  setGeneric("Burnout",
           function(beta1 = numeric(), beta2 = numeric(), MaxIncen = numeric(), LoanAge = numeric())
             {standardGeneric("Burnout")})

  setGeneric("Seasonality",
           function(alpha = numeric(), Month = numeric(), theta = numeric())
             {standardGeneric("Seasonality")})
  
  setGeneric("Prepayment.Model",
           function(ModelTune = "character", LoanAge = numeric(), 
                    Month = numeric(), incentive = numeric(), Burnout.maxincen = numeric())
             {standardGeneric("Prepayment.Model")})  

  setGeneric("DollarRoll", function(bond.id = "character", price = numeric(), drop = numeric(), original.bal = numeric(), 
                         settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(), finance.rate = numeric(), MortgageCashFlow = "character")
            {standardGeneric("DollarRoll")})
  
  setGeneric("DollarRollAnalytics", function(bond.id = "character", original.bal= numeric(), price = numeric(), drop = numeric(), trade.date = "character", 
                        settlement.date = "character", fwd.settlement.date = "character", reinvestment.rate = numeric(), finance.rate = numeric(), method = "ns", 
                        PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
            {standardGeneric("DollarRollAnalytics")})
  
  setGeneric("Mtg.Scenario", function(bond.id ="character", trade.date = "character", settlement.date = "character", price = numeric(), proceeds = numeric(),
                                      spot.spread = numeric(), original.bal = numeric(), scenario.set = vector(), rates.data = "character", 
                                      method = "character", PrepaymentAssumption = "character",..., ModelTune = "character", Burnout = numeric(),
                                      begin.cpr = numeric(), end.cpr = numeric(), seasoning.period = numeric(), CPR = numeric())
  {standardGeneric("Mtg.Scenario")})
  

  
  setMethod("show",
            signature(object = "MortgageCashFlows"),
            function (object) 
            {      
              cat("Bond Description", "\n")
              cat("BondId:"); print(object@ID)
              cat("Cusip:"); print(object@Cusip)
              cat("Coupon:"); print(object@Coupon)
              cat("Frequency:"); print(object@Frequency)
              cat("Basis:"); print(object@BondBasis)
              cat("Issue Date:"); print(object@IssueDate)
              cat("Last Payment Date:"); print(object@LastPmtDate)
              cat("Next Payment Date:"); print(object@NextPmtDate)
              cat("Maturity Date:"); print(object@Maturity)
              cat("Bond Valuation:", "\n")
              cat("Price:"); print(object@Price)
              cat("Accrued:"); print(object@Accrued)
              cat("Yield to Maturity:"); print(object@YieldToMaturity)
              cat("Risk Metrics:", "\n")
              cat("Weighted Average Life:"); print(object@WAL)
              cat("Modified Duration:"); print(unname(object@ModDuration))
              cat("Convexity:"); print(unname(object@Convexity))
              cat("Sector Detail:", "\n")
              cat("Bond Type:"); print(object@BondType)
              cat("Sector:"); print(object@Sector)
              cat("Moodys:"); print(object@Moody)
              cat("S&P:"); print(object@SP)
              cat("BondLab Rating:");print(object@BondLab)
              
              
              plotdata = as.data.frame(cbind(object@Period, object@ScheduledPrin, object@PrepaidPrin, 
                                             object@PassThroughInterest, object@ServicingIncome, object@PMIPremium, object@GFeePremium))
              colnames(plotdata) <- c("Period", "Scheduled Prin", "Prepaid Prin", "PT Interest", "Servicing", "PMI", "GFee")
              plotdata = melt(plotdata, id = "Period")
              
              plot <- ggplot(plotdata, aes(x= Period, y = value, fill = variable)) +
                geom_area() +
                theme_minimal()+
                scale_fill_brewer(palette = "Greys") +
                labs(fill = "") +
                ylab("Pool Cash Flow") +
                xlab("Period") +
                theme(axis.title.y=element_text(angle = 90, size = 20)) +
                theme(axis.text.y = element_text(angle = 90, size = 15)) +
                theme(axis.title.x=element_text(angle = 0, size = 20)) +
                theme(axis.text.x = element_text(angle = 0, size = 15)) +
                theme(legend.position = c(.82,.73))+
                theme(legend.background = element_rect(fill = "white"))
              
              print(plot)
            }
  )
  
 
  
  setMethod("show", 
            signature(object = "PassThroughAnalytics"),
            function(object)
            {
              cat("Bond Description", "\n")
              cat("BondId:"); print(object@ID)
              cat("Cusip:"); print(object@Cusip)
              cat("Coupon:"); print(object@Coupon)
              cat("Frequency:"); print(object@Frequency)
              cat("Basis:"); print(object@BondBasis)
              cat("Issue Date:"); print(object@IssueDate)
              cat("Last Payment Date:"); print(object@LastPmtDate)
              cat("Next Payment Date:"); print(object@NextPmtDate)
              cat("Maturity Date:"); print(object@Maturity)
              cat("Bond Valuation:", "\n")
              cat("Price:"); print(object@Price)
              cat("Accrued:"); print(object@Accrued)
              cat("Yield to Maturity:"); print(object@YieldToMaturity)
              cat("Risk Metrics:", "\n")
              cat("Weighted Average Life:"); print(object@WAL)
              cat("Modified Duration:"); print(unname(object@ModDuration))
              cat("Convexity:"); print(unname(object@Convexity))
              cat("Effective Duration"); print(unname(object@EffDuration))
              cat("Effective Convexity"); print(unname(object@EffConvexity))
              cat("Sector Detail:", "\n")
              cat("Bond Type:"); print(object@BondType)
              cat("Sector:"); print(object@Sector)
              cat("Moodys:"); print(object@Moody)
              cat("S&P:"); print(object@SP)
              cat("BondLab Rating:");print(object@BondLab)
              
              plotdata1 = as.data.frame(cbind(object@Period, object@TotalCashFlow))
              colnames(plotdata1) <- c("Period", "CashFlow")
              
              plot1 <- ggplot(plotdata1, aes(x= Period, y = CashFlow)) +
                geom_bar(stat = "identity", width = 1, fill = "Grey") +
                theme_minimal() + 
                labs(fill = "") +
                ylab("Mtg. Cash Flow") +
                xlab("Period") +
                theme(axis.title.y=element_text(angle = 90, size = 20)) +
                theme(axis.text.y = element_text(angle = 90, size = 15)) +
                theme(axis.title.x=element_text(angle = 0, size = 20)) +
                theme(axis.text.x = element_text(angle = 0, size = 15)) +
                theme(legend.position = c(.82,.73))
              
              plotdata2 = as.data.frame(cbind(object@KeyRateTenor, object@KeyRateDuration))
              colnames(plotdata2) <- c("KRTenor", "KRDuration")
              
              plot2 <- ggplot(plotdata2, aes(x = as.factor(KRTenor), y = KRDuration)) +
                geom_bar(stat = "identity", fill = "Grey") +
                theme_minimal() +
                labs(fill = "") +
                ylab("Key Rate Duration") +
                xlab("Key Rate Tenor") +
                theme(axis.title.y=element_text(angle = 90, size = 20)) +
                theme(axis.text.y = element_text(angle = 90, size = 15)) +
                theme(axis.title.x=element_text(angle = 0, size = 20)) +
                theme(axis.text.x = element_text(angle = 0, size = 15)) +
                theme(legend.position = c(.82,.73))
              
              multiplot(plot1, plot2, cols = 1)
              
            }
  )
  
  setMethod("show",
            signature(object = "DollarRoll"),
            function(object){
              cat("Dollar Roll Analysis", "\n")
              cat("Settlement Date"); print(object@SettlementDate)
              cat("Settlement Price"); print(object@Price)
              cat("Drop 32nds"); print(object@Drop)
              cat("Forward Settlement Date"); print(object@FwdSettlementDate)
              cat("Forward Price"); print(object@FwdPrice)
              cat("Beginning Market Value"); print(object@PrincipalProceeds)
              cat("Accrued Interest"); print(object@Accrued)
              cat("Roll Proceeds"); print(object@TotalProceeds)
              cat("Reinvestment Proceeds"); print(object@ReinvestmentIncome)
              cat("Future Value"); print(object@FutureValueRoll)
              cat("Dollar Advantage"); print(object@Advantage)
              #cat("Basis Points (Annualized"); print(object@BasisPoints)
              cat("Breakeven Drop"); print(object@DropImpliedValue)
              cat("Hold or Roll"); print(object@HoldorRoll)
              cat("Scheduled Principal"); print(object@ScheduledPrin)
              cat("Prepaid Principal"); print(object@PrepaidPrin)
              cat("Pass Through Interest"); print(object@PassThroughInterest)
              cat("Remaining Principal"); print(object@RemainingBalance)
              cat("Proceeds"); print(object@FuturePrincipalProceeds)
              cat("Hold Accrued"); print(object@FwdAccrued)
              cat("Future Value"); print(object@FutureValuePrinCarry)
            }
  )
  
  
  
  
