
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.

  # The following script is used to calculate Term Structure
  # metrics for mortgage backed securities.  To create the script
  # the standard procedure is followed set class, set generics,
  # set methods, functions.  This class is a sub class (document superclass)

  #' @include TermStructure.R
  NULL

  #' An S4 class MortgageTermStructure
  #' 
  #' @slot SpotSpread A numeric value the spread to the spot rate curve
  #' @slot EffDuration A numeric value the effective duration
  #' @slot EffConvexity A numeric value the effective convexity
  #' @slot KeyRateTenor A numeric value the Key Rate Tenor
  #' @slot KeyRateDuration A numeric value the Key Rate Duration
  #' @slot KeyRateConvexity A numeric value the Key Rate Convexity
  #' @importFrom stats approx
  #' @exportClass MortgageTermStructure
  setClass("MortgageTermStructure",
         representation(
           SpotSpread = "numeric",   
           EffDuration = "numeric",
           EffConvexity = "numeric",
           KeyRateTenor = "numeric",
           KeyRateDuration = "numeric",
           KeyRateConvexity = "numeric"))


  #' A standard generic function to access the slot SpotSpread
  #' @param object an S4 class object
  #' @export SpotSpread
  setGeneric("SpotSpread", function(object)
    {standardGeneric("SpotSpread")})
  
  #' A standard generic function to access the slot EffDuration
  #' @param object an S4 class object
  #' @export EffDuration
  setGeneric("EffDuration", function(object)
    {standardGeneric("EffDuration")})
  
  #' A standard generic function to access the slot EffConvexity
  #' @param object an S4 class object
  #' @export EffConvexity
  setGeneric("EffConvexity", function(object)
    {standardGeneric("EffConvexity")})
  
  #' A standard generic function to access the slot KeyRateTenor
  #' @param object an S4 class object
  #' @export KeyRateTenor
  setGeneric("KeyRateTenor", function(object)
    {standardGeneric("KeyRateTenor")})
  
  #' A standard generic function to access the slot KeyRateDuration
  #' @param object an S4 class object
  #' @export KeyRateDuration
  setGeneric("KeyRateDuration", function(object)
    {standardGeneric("KeyRateDuration")})
  
  #' A standard generic function to access the slot KeyRateConvexity
  #' @param object an S4 class object
  #' @export KeyRateConvexity
  setGeneric("KeyRateConvexity", function(object)
    {standardGeneric("KeyRateConvexity")})
  
  setMethod("initialize",
         signature("MortgageTermStructure"),
         function(.Object,
                  SpotSpread = "numeric",   
                  EffDuration = "numeric",
                  EffConvexity = "numeric",
                  KeyRateTenor = "numeric",
                  KeyRateDuration = "numeric",
                  KeyRateConvexity = "numeric",
                  ...){
          callNextMethod(.Object,
                  SpotSpread = SpotSpread,
                  EffDuration = EffDuration,
                  EffConvexity = EffConvexity,
                  KeyRateTenor = KeyRateTenor,
                  KeyRateDuration = KeyRateDuration,
                  KeyRateConvexity = KeyRateConvexity,
           ...)
         })
  
  #' Method to extract SpotSpread from S4 class
  #' @param object The name of the S4 object of type MortgageTermStructure
  #' @exportMethod SpotSpread
  setMethod("SpotSpread", signature("MortgageTermStructure"),
            function(object){object@SpotSpread})
  
  #' Method to extract EffDuration from S4 class
  #' @param object The name of the S4 object of type MortgageTermStructure
  #' @exportMethod EffDuration
  setMethod("EffDuration", signature("MortgageTermStructure"),
            function(object){object@EffDuration})
  
  #' Method to extract EffConvexity from S4 class
  #' @param object The name of the S4 object of type MortgageTermStructure
  #' @exportMethod EffConvexity
  setMethod("EffConvexity", signature("MortgageTermStructure"),
            function(object){object@EffConvexity})
  
  #' Method to get the KeyRateTenor from S4 class
  #' @param object The name of the S4 object of type MortgageTermStructure
  #' @exportMethod KeyRateTenor
  setMethod("KeyRateTenor", signature("MortgageTermStructure"),
            function(object){object@KeyRateTenor})
  
  #' Method to extract the KeyRateDuration from S4 class
  #' @param object The name of the S4 object of type MortgageTermStructure
  #' @exportMethod KeyRateDuration
  setMethod("KeyRateDuration", signature("MortgageTermStructure"),
            function(object){object@KeyRateDuration})
  
  #' Method to extract the KeyRateConvexity from S4 class
  #' @param object The name of the S4 object of type MortgageTermStructure
  #' @exportMethod KeyRateConvexity
  setMethod("KeyRateConvexity", signature("MortgageTermStructure"),
            function(object){object@KeyRateConvexity})
  
  #' A function to calculate mortgage key rate duration and convexity
  #' 
  #' This is a generic function used to construct the 
  #' MortgageTermStructure object
  #' @param bond.id A character string referencing an object of type MBS details
  #' @param original.bal A numeric value the original balance
  #' @param Rate.Delta A numeric value the rate delta used to calculate KRDs
  #' @param TermStructure A character string referencing an object of the 
  #' type TermStructure
  #' @param settlement.date a character string the settlement date
  #' @param principal A numeric value the principal balance.  The principal 
  #' balance is the original balance multiplied by the MBS factor
  #' @param price A numeric value the price paid
  #' @param cashflow A character string referencing an object of the 
  #' type MortgageCashFlow
  #' @export MtgTermStructure
  MtgTermStructure <- function(bond.id = "character", 
                             original.bal = numeric(), 
                             Rate.Delta = numeric(), 
                             TermStructure = "character", 
                             settlement.date = "character", 
                             principal = numeric(), 
                             price = "character", 
                             cashflow = "character"){
  
  # Open connection to the prepayment model tuning library
  ModelTune <- ModelTune(bond.id = bond.id)
  Burnout = BurnOut(bond.id)
  
  # Open connection to the Mortgage Model function
  MortgageRate <- MtgRate()
  
  #Call the bond frequency to adjust the spot spread to the payment 
  #frequency of the bond
  frequency = Frequency(bond.id)
  maturity = Maturity(bond.id)
  accrued = Accrued(cashflow)

  # create PriceTypes for mortgage cash flow call in key rate
  Price <- PriceTypes(price = price)
 
  # calcuate proceeds the order of operation is important

  proceeds = (principal * PriceBasis(Price)) + accrued

  #========== Set the functions that will be used ==========
  # These functions are set as internal functions to key rates
  # this insures that stored values will not be wrongly be passed to the 
  # function internal functions used to compute key rate duration and convexity
  
  EffectiveMeasures <- function(rate.delta , 
                                cashflow, 
                                cashflow.up, 
                                cashflow.dwn, 
                                discount.rates, 
                                discount.rates.up, 
                                discount.rates.dwn, 
                                t.period, 
                                type){
    Price.NC = sum((1/((1+discount.rates)^t.period)) * cashflow)
    Price.UP = sum((1/((1+discount.rates.up)^t.period)) * cashflow.up)
    Price.DWN = sum((1/((1+discount.rates.dwn)^t.period)) * cashflow.dwn)
    
    switch(type,

  duration = (Price.UP - Price.DWN)/(2*Price.NC*rate.delta),
  convexity =  (Price.UP + Price.DWN - (2*Price.NC))/
    (2 * Price.NC * (rate.delta^2))
  )}
  
  
  # The spot spread function is used to solve for the spread to the spot 
  # curve to normalize discounting
  Spot.Spread <- function(spread = numeric(), 
                          cashflow = vector(), 
                          discount.rates = vector(), 
                          t.period = vector(), 
                          proceeds = numeric()){
    Present.Value <- sum((1/(1+(discount.rates + spread))^t.period) * cashflow)
    return(proceeds - Present.Value)
  }
  
  # set up the index names for each array that will be used in the function
  # Index names set names for columns in the KRIndex. This tables set the 
  # control strucutre for the loop that will compute key rate duration given 
  # rates in the key rate table
  Index.Names <- c("Period", 
                   "Time", 
                   "Spot Curve", 
                   "Disc Curve", 
                   "KRDwn", 
                   "KRUp")
  
  # KR.Duration.Col set the column names
  KR.Duration.Col <- c("Key Rate", 
                       "Key Rate Duration", 
                       "Key Rate Convexity")
  
  #sets the tenor of the key rate that will report a duration
  KR.Duration.Row <- c("0.25", 
                       "1", 
                       "2", 
                       "3", 
                       "5", 
                       "7", 
                       "10", 
                       "15", 
                       "20", 
                       "25", 
                       "30",
                       "40")
  
  # set the arrays for key rate duration calculation
  # key rate table holds data for the term structure and shifts in the key rates
  # the array dim is 600 periods this will allow the calculation of 40 year
  # pass-through bonds.  The 30-year key rate is interpolated between the 
  # 20-year (240) the 40-year (480).  The 40-year is interpolated between the 
  # 30-year (360) and the 50-year (600) 
  Key.Rate.Table <- array(data = NA, 
                          c(600,6), 
                          dimnames = list(seq(c(1:600)), 
                                          Index.Names))
  
  #key rate duration array holds the key rates and the key rate duration
  KR.Duration <- array(data = NA, c(12,3), 
                       dimnames = list(seq(c(1:12)), 
                                       KR.Duration.Col))
  
  KR.Duration[,"Key Rate"] <- as.numeric(KR.Duration.Row)
  
  # Create Index for Key Rate Table for interpolation of Key Rate Duration set 
  # outer knot points the outer points are the first and last elements in KR.  The
  # key rate count is 14. Of these, 2 through 12 are reported see KR.Duration.Row 
  KR <- c("0.083", 
          "0.25", 
          "1", 
          "2", 
          "3", 
          "5", 
          "7", 
          "10", 
          "15", 
          "20", 
          "25", 
          "30", 
          "40",
          "50")   
 
  # Dimension the key rate index array.  The array will be used to manage the
  # shifts along the curve.  Refer to index names above for the mapping of the 
  # columns
  KRCount = length(KR)
  KRIndex <- array(data = NA, c(KRCount, 6), 
                   dimnames = list(seq(c(1:KRCount)), Index.Names))
  
  # Initialize the cash flow array for discounting and spot rate caclulations
  # this will be populated from class MortgageCashFlows
  # !!! DIM CASHFLOW ARRAY TO SIZE OF CASHFLOW!!
  # This will be the first pass at the cash flows before shocking the curve
  
  # Dimension the cashflow array for key rate discounting
  CashFlowArray <- array(data = NA, c(600,4), 
                         dimnames = list(seq(1:600), 
                                         c("period", 
                                           "cashflow_nc", 
                                           "cashflow_dwn", 
                                           "cashflow_up")))
  
  # Initialze the spot rate array for key rate duration calculations
  # The spot rate must be passed from Term Strucuture object
  SpotRate <- as.matrix(SpotRate(TermStructure))
  
  # Populate Period, Time(t) and Spot Rate Curve of Key Rate Table using NS 
  # coefficients from Term Stucture and then populate and align the cashflow 
  # array for discounting and key rate computations

  for(x in 1:600){
    #Period (n) in which the cashflow is received
    Key.Rate.Table[x,"Period"] = x
    
  # Time (t) at which the cashflow is received
  # Time period in which the cashflow was received for discounting
    Key.Rate.Table [x,"Time"] = x/months.in.year

  #spot rates for discounting
  Key.Rate.Table[x,"Spot Curve"] = SpotRate[x,1]/yield.basis

  #Align Cash Flows and populated the CashFlowArray
  #Step One: Make sure all cash flows are set to zero
  CashFlowArray[x,"period"] = Key.Rate.Table[x,"Time"]
  CashFlowArray[x,"cashflow_nc"] = 0
  }
  
  # Step Two: Initialize loop and set the cashflows in the array 
  # This loops through the time period and set the cashflows into the 
  # proper array location for discounts by indexing the cashflows to the array.  
  # The indexing is conditional on the integer of the first period less than or 
  # equal to 1
  
  # This code populates the cashflows from the cashflow array object which 
  # is MortgageTermStructure via cashflows input
  
  if(as.integer(cashflow@TimePeriod[1] *12) != 1) 
    CashFlowArray[as.integer(cashflow@TimePeriod * 12) + 1,"cashflow_nc"] = 
    cashflow@TotalCashFlow
  
  if(as.integer(cashflow@TimePeriod[1] * 12) == 1) 
    CashFlowArray[as.integer(cashflow@TimePeriod * 12),"cashflow_nc"] = 
    cashflow@TotalCashFlow
  
  # This code solves for the spread to spot curve to equal price
  # This code can be replaced with the curve spreads class 
  spot.spread <- uniroot(Spot.Spread, 
                         interval = c(-.75, .75), 
                         tol = .0000000001, 
                         CashFlowArray[,"cashflow_nc"],
                         discount.rates = Key.Rate.Table[,"Spot Curve"], 
                         t.period = Key.Rate.Table[,"Time"] , 
                         proceeds)$root

  # The spot spread does not need to be converted to semi-bond 
  # because time weights are used rather than period weights
  
  # Step three add the spot spread to the spot curve 
  # This yields the discount rates that are need for the key rate duration
  # calculation
  # at a minimum the cash flow array should be 360 months
  
  for(i in 1:600){
    Key.Rate.Table[i,"Disc Curve"] = 
      Key.Rate.Table[i,"Spot Curve"] + spot.spread
    }
  
  #========= Populate KRIndex Table =========================
  # The key rate index table will serve as the control table for the looping
  # using this table allows for incremental looping of discontinous 
  # segments of the
  # spot rate curve and is proprietary to bondlab

  # Step 1 populate Period (n)
  KRIndex[1:KRCount,"Period"] <- round(as.numeric(KR) * months.in.year,0)
  
  # Step 2 populate time period (t)
  KRIndex[1:KRCount,"Time"] <- as.numeric(KR)                    
  
  # Step 3 Populate Index Table with the relevant points on the spot curve
  # this is done by looping through the key rate table and allows for term 
  # structure implementation other than Nelson Siegel the key rate index table
  # (KRIndex) is used to populate the key rate table (KRTable)
  for (j in 1:KRCount){                                   
    for (i in 1:600){
      if (Key.Rate.Table[i,"Period"] == round(KRIndex[j,"Time"] * 12,0)) {
        KRIndex[j,"Spot Curve"] = Key.Rate.Table[i,"Spot Curve"]
        } else {KRIndex[j,"Spot Curve"] = KRIndex[j,"Spot Curve"]}
    }
  }
  
  # Step 4 Populate KRIndex Table with the appropriate Discount Curve values 
  # from the key rate table these will be the reference points for the 
  # appropriate key rate shifts
  for (j in 1:KRCount){                                   
    for (i in 1:600){
      if (Key.Rate.Table[i,"Period"] == round(KRIndex[j,"Time"] * 12,0)) {
        KRIndex[j,"Disc Curve"] = Key.Rate.Table[i,"Disc Curve"]
        } else {KRIndex[j,"Spot Curve"] = KRIndex[j,"Spot Curve"]}
    }
  }

  # Step 5 Populated KRIndex Table with KR Shifts

  for (j in 1:KRCount){
    KRIndex[j,"KRDwn"] = KRIndex[j,"Disc Curve"] - (Rate.Delta/yield.basis)
    KRIndex[j,"KRUp"] = KRIndex[j,"Disc Curve"] + (Rate.Delta/yield.basis)
  }
  
  #===== Implement Shift of Spot Rates =======================
  # Once the KRIndex is populated implement the shift in the spot rates using 
  # the KRIndex table as the control 
  # w is the counter of the internal knot points used to compute key rate 
  # duration it ignores the boundary knots
  # used for interpolation at the end points.  x is the length of the array.
  # Currently the analysis is limited
  # to loans (bonds) with a maximum of 30-years to maturity.  This can be made
  # dynamic at some point in the future
  # y is column counter used the key rate down and key rate up values
  for (w in 2:(KRCount-1)){ 
    for (x in 1:600){
  #This allocates the up and down Key.Rate.Table index.  
  #col 5 is KR down and 6 is KR up
  for(y in 5:6){
  # step 1: populate the spot curve outside the key rate shift =========
  if(Key.Rate.Table[x,"Time"] <= KRIndex[w-1,2] || 
     Key.Rate.Table[x,"Time"] >= KRIndex[w+1,2]) 
  {Key.Rate.Table[x,y] = Key.Rate.Table[x,"Disc Curve"]
  } else {Key.Rate.Table[x,y] = 0}
  }
      }

 #===== Begin Interpolation of Spot Curve =====================
    # Maturity points on the spot rate curve to interpolate
    KRx <- c(KRIndex[w-1,"Time"], 
             KRIndex[w,"Time"], 
             KRIndex[w+1,"Time"]) 
    
    # Spot rates that correspond to the maturity points Down and Up
    for(z in 1:2){                                       
      if (z == 1) 
      {KRy <- c(KRIndex[w-1,"Disc Curve"], 
                KRIndex[w,"KRDwn"], 
                KRIndex[w+1,"Disc Curve"])
      } else {
        KRy <- c(KRIndex[w-1,"Disc Curve"], 
                KRIndex[w,"KRUp"], 
                KRIndex[w+1,"Disc Curve"])}
      
      a = KRIndex[w-1,"Period"] #+ Rate.Delta 
      b = KRIndex[w+1,"Period"] #- Rate.Delta
      for(h in a : b){
        Key.Rate.Table[h,(z+4)] <- approx(KRx,KRy, Key.Rate.Table[h,"Time"])$y
      } # Loop through Key Rate Table and interpolation
    } # Inner Loop to set interpolation points from KRIndex
    
  # This line sets the end points for discounting when the 30-year is last point 
  # on the curve.  It is possible to set the endpoints longer using row 12 of 
  # the KRIndex
    
  #if (KRIndex[w,2] == 30) {
  #  (Key.Rate.Table[x,"KRDwn"] = KRIndex[12,"KRDwn"]) & 
  #    (Key.Rate.Table[x,"KRUp"] = KRIndex[12,"KRUp"])}
    
  # Derive Key Rate Up and Key Rate Down Cash Flows
  # For MBS Cashflows two term strucutre objects are created the 
  # up and down term strucutres
  # are used to drive the key rate scenario foward rates and the 
  # up and down MBS cashflows

  # Initialize the TermStructure Up and Down objects 
  # Use the term structure object
  # 24 is 2-year forward
  # 120 is 10-year forward
  # create variables and replace 24 and 120 in code
    
  
    Key.Rate.TS.Dwn <- new("TermStructure",
                           TradeDate <- TradeDate(TermStructure),
                           Period <- Period(TermStructure),
                           Date <- ForwardDate(TermStructure),
                           SpotRate = numeric(),
                           ForwardRate = numeric(),
                           TwoYearFwd = numeric(),
                           TenYearFwd = numeric())
    
    Key.Rate.TS.Up <- new("TermStructure",
                           TradeDate <- TradeDate(TermStructure),
                           Period <- Period(TermStructure),
                           Date <- ForwardDate(TermStructure),
                           SpotRate = numeric(),
                           ForwardRate = numeric(),
                           TwoYearFwd = numeric(),
                           TenYearFwd = numeric()) 

  #SpotRate(Key.Rate.TS.Dwn) <- c((Key.Rate.Table[,"KRDwn"]-spot.spread) * 100, 
  #((SpotRate(TermStructure)[361:492])))
  
  # This section of code sets up the prepayment model to respond across the
  # key rate simulations creating a unique vector for each key rate shock
  # Multiply by 100 (rate.basis) to pass the rates from termstructure to 
  # prepayment model in the correct basis.
  SpotRate(Key.Rate.TS.Dwn) <- (Key.Rate.Table[,"KRDwn"]-spot.spread) * 100
    
  TwoYearForward(Key.Rate.TS.Dwn) <- Forward.Rate(
    SpotRate.Curve = SpotRate(Key.Rate.TS.Dwn), 
    FwdRate.Tenor = 24) 
    TenYearForward(Key.Rate.TS.Dwn) <- Forward.Rate(
      SpotRate.Curve = SpotRate(Key.Rate.TS.Dwn),
      FwdRate.Tenor = 120)
    
  #SpotRate(Key.Rate.TS.Up) <- c((Key.Rate.Table[,"KRUp"]-spot.spread) * 100, 
  #((SpotRate(TermStructure)[361:492])))
  
  SpotRate(Key.Rate.TS.Up) <- (Key.Rate.Table[,"KRUp"]-spot.spread) * 100

  TwoYearForward(Key.Rate.TS.Up) <- Forward.Rate(
    SpotRate.Curve = SpotRate(Key.Rate.TS.Up),
    FwdRate.Tenor = 24) 
    TenYearForward(Key.Rate.TS.Up) <- Forward.Rate(
      SpotRate.Curve = SpotRate(Key.Rate.TS.Up),
      FwdRate.Tenor = 120)
    
  # Run the prepayment model to derive the SMM vector given each Key Rate shift
  # =======================================================================   
  # Key Rate Shift Down Prepayment Model and CashFlows
  # ======================================================================
    Prepayment.Dwn <- PrepaymentModel(
      bond.id = bond.id,
      MortgageRate = MortgageRate,
      TermStructure = Key.Rate.TS.Dwn,
      PrepaymentAssumption = "MODEL",
      ModelTune = ModelTune,
      Burnout = Burnout,
      Severity = 0) 
    
  # Mortgage Cashflows call here requires that price as whole number passed
  # Always use sprintf("%.8f", PriceDecimal(Price)) to convert back to string
  # and preserve the trailing zeros that are needed for the PriceTypes function
  # when price when the price tail is .000 or "-00"
  MortgageCashFlows.Dwn <- MortgageCashFlow(
    bond.id = bond.id,
    original.bal = original.bal,
    settlement.date = settlement.date,
    price = sprintf("%.8f", PriceDecimal(Price)), 
    PrepaymentAssumption = Prepayment.Dwn)
    
    # Assign CashFlows into the cash flow array.  This has to be done in a loop
    for(cfd in 1:600){
      if(cfd > as.numeric(length(MortgageCashFlows.Dwn@TotalCashFlow))) {
        CashFlowArray[cfd,"cashflow_dwn"] = 0
      } else {
          CashFlowArray[cfd,"cashflow_dwn"] = 
            MortgageCashFlows.Dwn@TotalCashFlow[cfd]}
    }
  # ============================================================  
  # Key Rate Shift Up Prepayment Model and CashFlows
  # ==============================================================
  Prepayment.Up <- PrepaymentModel(
    bond.id = bond.id,
    MortgageRate = MortgageRate,
    TermStructure = Key.Rate.TS.Up,
    PrepaymentAssumption = "MODEL",
    ModelTune = ModelTune,
    Burnout = Burnout,
    Severity = 0)
    
  # Mortgage Cashflows call here requires that price as 
  # a whole number passed
  MortgageCashFlows.Up <- MortgageCashFlow(
    bond.id = bond.id,
    original.bal = original.bal,
    settlement.date = settlement.date,
    price = sprintf("%.8f", PriceDecimal(Price)), #price.mtg.cashflow,
    PrepaymentAssumption = Prepayment.Up)

  # Assign CashFlows into the cash flow array. 
  # This has to be done in a loop
  for(cfu in 1:600){
    if(cfu > as.numeric(length(MortgageCashFlows.Up@TotalCashFlow))) {
      CashFlowArray[cfu,"cashflow_up"] = 0
    } else {
        CashFlowArray[cfu,"cashflow_up"] = 
          MortgageCashFlows.Up@TotalCashFlow[cfu]}
  }
  
 # Calculate Key Rate Duration 
    KR.Duration[w-1,2] <- -EffectiveMeasures(
      rate.delta = Rate.Delta/100, 
      cashflow = CashFlowArray[,"cashflow_nc"],
      cashflow.dwn = CashFlowArray[,"cashflow_dwn"],
      cashflow.up = CashFlowArray[,"cashflow_up"],
      discount.rates = Key.Rate.Table[,"Disc Curve"], 
      discount.rates.up = Key.Rate.Table[,"KRUp"],
      discount.rates.dwn = Key.Rate.Table[,"KRDwn"],
      t.period = Key.Rate.Table[,"Time"],
      type = "duration"
    ) 
    
    KR.Duration[w-1,3] <- EffectiveMeasures(
      rate.delta = Rate.Delta /100, 
      cashflow = CashFlowArray[,"cashflow_nc"],
      cashflow.dwn = CashFlowArray[,"cashflow_dwn"],
      cashflow.up = CashFlowArray[,"cashflow_up"],
      discount.rates = Key.Rate.Table[,"Disc Curve"], 
      discount.rates.up = Key.Rate.Table[,"KRUp"],
      discount.rates.dwn = Key.Rate.Table[,"KRDwn"],
      t.period = Key.Rate.Table[,"Time"],
      type = "convexity"
    )
    
  } # Outer Loop around KRIndex
  new("MortgageTermStructure",
      SpotSpread = spot.spread * 100,
      EffDuration = sum(KR.Duration[,"Key Rate Duration"]),
      EffConvexity = sum(KR.Duration[,"Key Rate Convexity"]),
      KeyRateTenor = unname(KR.Duration[,"Key Rate"]),
      KeyRateDuration = unname(KR.Duration[,"Key Rate Duration"]),
      KeyRateConvexity = unname(KR.Duration[,"Key Rate Convexity"])
  )
} # End the function