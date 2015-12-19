  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # Bond Lab License
  # Copyright (C) 2015 Bond Lab Technologies, Inc

  #' @include MortgageCashFlow.R MortgageCashFlowArray.R
  NULL
  
  #' An alternative function to compute pool cash flow metrics
  #' 
  #' This function by passes the mortgage cash flow engine used in the MortgageCashFlow function
  #' thereby allowing the user to create a custom cashflow array possible use of this function is the loan level
  #' agregation of either pools or REMIC cash flow.
  #' 
  #' The cashflow array passed to this function must be n rows by 23 columns the file must have headers for each column
  #' as follows: Period, Date, Time, Begin Bal, Monthly Pmt, Scheduled Int, Scheduled Prin, Prepaid Prin, Defaulted Prin,
  #' Loss Amount, Recovered Amount, Ending Bal, Servicing, PMI, GFee, Pass Through Interest, Investor CashFlow, 
  #' Present Value Factor, Present Value, Duration, Convexity Time, CashFlow Convexity, Convexity.  Values in the array are referenced
  #' by name thus the data may be presented in any order however the column headers must match the above exactly.
  #' @param CashFlowData a character string referencing the cash flow file whose extension is .cfm
  #' @param price the price of the MBS. Price is input as a whole number for example 102 is input as 102 not 1.02
  #' @param original.bal the original balance of the MBS pool
  #' @param settlement.date the settlement date of the MBS trade
  #' @param bond.id a character string the bond.id
  #' @examples 
  #' \dontrun{MortgageCashFlow_KDS(CashFlowData = "blx_test", price = 100, settlement.date = "01-10-2013", 
  #' original.bal = 102, bond.id = "bondlabMBS4")}
  #' @export MortgageCashFlow_Array
  MortgageCashFlow_Array <- function(CashFlowData = "character",
                                 price = numeric(),
                                 original.bal = numeric(),
                                 settlement.date = "character",
                                 bond.id = "character"){ 
  # ----------------------------------------------------------------------
  # Parse the cashflow file read into CashFlow Data
  # ----------------------------------------------------------------------
    ReadCashFlow <- "sed -i ''  '/^[\r#]/d; /AGENCY/d' %s/Temp_CashFlow/%s.cfm"
    
    cmd <- sprintf(ReadCashFlow, system.file(package = "BondLab"), CashFlowData)
    system(cmd)
    
    CashFlowData <- read.delim(
      paste(system.file(package = "BondLab"),"/Temp_CashFlow/", CashFlowData, ".cfm", sep =""), header = TRUE, sep ="") 
  # ----------------------------------------------------------------------
  # Find the number of periods in the cashflow array
  # Note: Quan reports cashflow array length based on original term thus a seasoned
  # MBS will have zero in principal column where the row number exceeds the 
  # remaining term.  This will return a NaN value that must be set to zero
  # -----------------------------------------------------------------------
  num.periods <- nrow(CashFlowData)
  
  # -----------------------------------------------------------------------
  # Get the pass through cusip
  # -----------------------------------------------------------------------
  bond.id <- MBS(MBS.id = bond.id)
  
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
  
  #  Validate the price and coupon passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(price <= 1) {price = price} else {price = price/price.basis}
  if(coupon > 1) {coupon = coupon/100} else {coupon = coupon}
  if(note.rate > 1) {note.rate = note.rate/100} else {note.rate = note.rate}
  
  # ----------------------------------------------------------------------
  #calculate beginning balance (principal) from the MBS pool factor
  # ----------------------------------------------------------------------
  factor = 1 #bond.id@MBSFactor
  principal = original.bal * factor
  
  # ----------------------------------------------------------------------
  # Build the vector of dates for the payment schedule
  # ----------------------------------------------------------------------
  pmtdate.interval = months.in.year/frequency
  
  pmtdate <- as.Date(c(if(settlement.date == issue.date) 
  {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))} else 
  {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y")
  
  # -----------------------------------------------------------------------
  # Define column names of the matrix
  # Note: Prepaid Prin column holds the sum or the scheduled and prepaid cash flow
  # the principal should be divided between scheduled and prepaid for cash flow graphs
  # -----------------------------------------------------------------------
  
  col.names <- c("Period", 
                 "Date", 
                 "Time", 
                 "Begin Bal", 
                 "Monthly Pmt", 
                 "Scheduled Int", 
                 "Scheduled Prin", 
                 "Prepaid Prin",
                 "Defaulted Prin",
                 "Loss Amount",
                 "Recovered Amount",
                 "Ending Bal", 
                 "Servicing", 
                 "PMI", 
                 "GFee", 
                 "Pass Through Interest", 
                 "Investor CashFlow", 
                 "Present Value Factor", 
                 "Present Value", 
                 "Duration", 
                 "Convexity Time", 
                 "CashFlow Convexity", 
                 "Convexity")
  # -----------------------------------------------------------------------
  # Build the time period vector (n) for discounting the cashflows 
  # nextpmt date is vector of payment dates to n for each period
  # -----------------------------------------------------------------------
  
  time.period = BondBasisConversion(issue.date = issue.date, 
                                    start.date = start.date, 
                                    end.date = end.date, 
                                    settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, 
                                    nextpmt.date = pmtdate)
  
  
  # Assign values to from CashFlowData to the CashFlow array
  CashFlow <- MortgageCashFlowArray(CashFlowArray = array(
    data = NA, 
    dim = c(num.periods, 23),
    dimnames = list(seq(c(1:num.periods)),col.names)))
  
  CashFlow@CashFlowArray[,"Period"] <- CashFlowData[,"month"]
  CashFlow@CashFlowArray[,"Date"] <- pmtdate + delay
  CashFlow@CashFlowArray[,"Time"] <- time.period
  CashFlow@CashFlowArray[,"Begin Bal"] <- 0
  CashFlow@CashFlowArray[,"Monthly Pmt"] <- 0
  CashFlow@CashFlowArray[,"Scheduled Int"] <- 0
  CashFlow@CashFlowArray[,"Scheduled Prin"] <- 0
  CashFlow@CashFlowArray[,"Prepaid Prin"] <- CashFlowData[,"principal"]
  CashFlow@CashFlowArray[,"Defaulted Prin"] <- 0
  CashFlow@CashFlowArray[,"Loss Amount"] <- 0
  CashFlow@CashFlowArray[,"Recovered Amount"] <- CashFlowData[,"loss"]
  CashFlow@CashFlowArray[,"Ending Bal"] <- 0
  CashFlow@CashFlowArray[,"Servicing"] <- 0
  CashFlow@CashFlowArray[,"PMI"] <- 0
  CashFlow@CashFlowArray[,"GFee"] <- 0
  CashFlow@CashFlowArray[,"Pass Through Interest"] <- CashFlowData[,"interest"]
  CashFlow@CashFlowArray[,"Investor CashFlow"] <- CashFlow@CashFlowArray[,"Pass Through Interest"] + 
    CashFlow@CashFlowArray[,"Prepaid Prin"] + CashFlow@CashFlowArray[,"Recovered Amount"] 
  CashFlow@CashFlowArray[,"Present Value Factor"] <- 0
  CashFlow@CashFlowArray[,"Present Value"] <- 0
  
  # -----------------------------------------------------------------------
  # Replace NaN with value of zero
  # ----------------------------------------------------------------------
  CashFlow@CashFlowArray[,][is.nan(CashFlow@CashFlowArray[,])] <- 0
  
  CFA <- CashFlow
  
  # ----------------------------------------------------------------------
  # Calculate accrued interest for the period
  # ----------------------------------------------------------------------
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
                                         start.date = start.date, 
                                         end.date = end.date, 
                                         settlement.date = settlement.date,
                                         lastpmt.date = lastpmt.date,
                                         nextpmt.date = nextpmt.date)) * days.in.year.360
  
  days.between.pmtdate = ((months.in.year/frequency)/months.in.year) * days.in.year.360
  days.of.accrued = (days.between.pmtdate - days.to.nextpmt) 
  accrued.interest = (days.of.accrued/days.between.pmtdate) * CashFlow@CashFlowArray[1,"Pass Through Interest"]
  
  
  # ----------------------------------------------------------------------
  # Solve for the yield to maturity
  # ---------------------------------------------------------------------
  
  irr <- function(rate,
                  time.period,
                  cashflow,
                  principal,
                  price,
                  accrued.interest){
    present.value = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(present.value) - (proceeds + accrued.interest)
  }
  
  ytm <- try(uniroot(irr,
                     interval = c(lower = -.75, upper = .75),
                     tol = tolerance,
                     time.period = CashFlow@CashFlowArray[,"Time"],
                     cashflow = CashFlow@CashFlowArray[,"Investor CashFlow"],
                     principal = principal,
                     price = price,
                     accrued.interest = accrued.interest)$root)
  
  Yield.To.Maturity = (((1 + ytm)^(1/frequency))-1) * frequency
  
  # -----------------------------------------------------------------
  # Calculate Discount Factors
  # Note: Test TimeValue function here
  # -----------------------------------------------------------------
  CashFlow@CashFlowArray[,"Present Value Factor"] = 1/((1+(Yield.To.Maturity/frequency))^(CashFlow@CashFlowArray[,"Time"] * frequency))
  
  # -----------------------------------------------------------------
  # Present Value of the Cash Flow
  # -----------------------------------------------------------------
  CashFlow@CashFlowArray[,"Present Value"] = CashFlow@CashFlowArray[,"Investor CashFlow"] * CashFlow@CashFlowArray[,"Present Value Factor"]
  
  # -----------------------------------------------------------------
  # Risk Measures Duration Factors
  # -----------------------------------------------------------------
  CashFlow@CashFlowArray[,"Duration"] = (CashFlow@CashFlowArray[,"Time"] * 
                                           CashFlow@CashFlowArray[,"Present Value"] / ((principal * price) + accrued.interest))
  
  # ----------------------------------------------------------------
  # Convexity Time Factors
  # ----------------------------------------------------------------
  CashFlow@CashFlowArray[,"Convexity Time"] = CashFlow@CashFlowArray[,"Time"] * (CashFlow@CashFlowArray[,"Time"] + 1)
  
  # ----------------------------------------------------------------
  # Convexity CashFlow
  # ----------------------------------------------------------------
  CashFlow@CashFlowArray[,"CashFlow Convexity"] = (CashFlow@CashFlowArray[,"Investor CashFlow"]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((CashFlow@CashFlowArray[,"Time"] + 2) * frequency)))/ 
    ((principal * price) + accrued.interest)
  
  # ----------------------------------------------------------------
  # Convexity
  # ----------------------------------------------------------------
  CashFlow@CashFlowArray[,"Convexity"] = CashFlow@CashFlowArray[,"Convexity Time"] * CashFlow@CashFlowArray[,"CashFlow Convexity"]
  
  # ---------------------------------------------------------------
  # Weighted Average Life
  # ---------------------------------------------------------------
  #Weighted Average Life
  WAL = sum(((CashFlow@CashFlowArray[,"Scheduled Prin"] + 
                CashFlow@CashFlowArray[,"Prepaid Prin"] + 
                CashFlow@CashFlowArray[,"Recovered Amount"]) * CashFlow@CashFlowArray[,"Time"]) / 
              sum(CashFlow@CashFlowArray[,"Scheduled Prin"] + CashFlow@CashFlowArray[,"Prepaid Prin"] + CashFlow@CashFlowArray[,"Recovered Amount"]))
  
  #Duration and Convexity
  Duration = apply(CashFlow@CashFlowArray, 2, sum)["Duration"]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(CashFlow@CashFlowArray, 2, sum)["Convexity"] * .5
  
  
  # Create the class Mortgage Loan Cashflows
  new("MortgageCashFlow",
      Price = price * price.basis,
      Accrued = accrued.interest,
      YieldToMaturity = Yield.To.Maturity,
      WAL = WAL,
      ModDuration = Modified.Duration,
      Convexity = Convexity,
      Period = CashFlow@CashFlowArray[,"Period"],
      PmtDate = as.character(as.Date(CashFlow@CashFlowArray[,"Date"], origin = "1970-01-01")),
      TimePeriod = CashFlow@CashFlowArray[,"Time"],
      BeginningBal = CashFlow@CashFlowArray[,"Begin Bal"],
      MonthlyPmt = CashFlow@CashFlowArray[,"Monthly Pmt"],
      MonthlyInterest = CashFlow@CashFlowArray[,"Scheduled Int"],
      PassThroughInterest = CashFlow@CashFlowArray[,"Pass Through Interest"],
      ScheduledPrin = CashFlow@CashFlowArray[,"Scheduled Prin"],
      PrepaidPrin = CashFlow@CashFlowArray[,"Prepaid Prin"],
      DefaultedPrin = CashFlow@CashFlowArray[,"Defaulted Prin"],
      LossAmount = CashFlow@CashFlowArray[,"Loss Amount"],
      RecoveredAmount = CashFlow@CashFlowArray[,"Recovered Amount"],
      EndingBal = CashFlow@CashFlowArray[,"Ending Bal"],
      ServicingIncome = CashFlow@CashFlowArray[,"Servicing"],
      PMIPremium = CashFlow@CashFlowArray[,"PMI"],
      GFeePremium = CashFlow@CashFlowArray[,"GFee"],
      TotalCashFlow = CashFlow@CashFlowArray[,"Investor CashFlow"])}


