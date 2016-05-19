
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # File License
  # Copyright (C) 2015 Bond Lab Technologies, Inc
  
  #' An S4 class MortgageCashFlow containing cashflow data 
  #' for a mortgage pass-through security
  #' 
  #' @slot Price A numeric value the price of the pass-through.
  #' @slot Accrued A numeric value the accrued interest as of settlement date.
  #' @slot YieldToMaturity A numeric value the yield to maturity.
  #' @slot WAL A numeric value the weighted average life of the pass-thorough.
  #' @slot ModDuration A numeric value the Modified Duration pass-through.
  #' @slot Convexity A numeric value the Convexity of the pass-through.
  #' @slot Period A numeric value the period in which the cash-flow is received.
  #' @slot PmtDate A character string the date in which the cash-flow is received.
  #' @slot TimePeriod A numeric value the time weight to apply to the principal cash-flow
  #' and discount factors.
  #' @slot BeginningBal A numeric value the Beginning Balance in the period.
  #' @slot MonthlyPmt A numeric value the borrower's monthly payment.
  #' @slot MonthlyInterest A numeric value the borrower's monthly interest.
  #' @slot PassThroughInterest A numeric value the pass-through interest paid
  #' to the investor in the pool.
  #' @slot ScheduledPrin A numeric value the scheduled principal due in the period.
  #' @slot PrepaidPrin A numeric value the prepaid principal in the period.
  #' @slot DefaultedPrin A numeric value the default principal in the period.
  #' @slot LossAmount A numeric value the loss amount in the period.
  #' @slot RecoveredAmount A numeric value the recovered amount in the period.
  #' @slot EndingBal A numeric value the ending balance in the period.
  #' @slot ServicingIncome A numeric value the servicing recevied in the period.
  #' @slot PMIPremium A numeric value the PMI paid in the period.
  #' @slot GFeePremium A numeric value the GFee paid in the period.
  #' @slot TotalCashFlow A numeric value the total cashflow paid in the period. 
  #' @exportClass MortgageCashFlow
  setClass("MortgageCashFlow",
         representation(
           Price = "numeric",
           Accrued = "numeric",
           YieldToMaturity = "numeric",
           WAL = "numeric",
           ModDuration = "numeric",
           Convexity = "numeric",
           Period = "numeric",
           PmtDate = "character",
           TimePeriod = "numeric",
           BeginningBal = "numeric",
           MonthlyPmt = "numeric",
           MonthlyInterest = "numeric",
           PassThroughInterest = "numeric",
           ScheduledPrin = "numeric",
           PrepaidPrin = "numeric",
           DefaultedPrin = "numeric",
           LossAmount = "numeric",
           RecoveredAmount = "numeric",
           EndingBal = "numeric",
           ServicingIncome = "numeric",
           PMIPremium = "numeric",
           GFeePremium = "numeric",  
           TotalCashFlow = "numeric"))
  
  setGeneric("MortgageCashFlow", function(bond.id = "character", 
                                        original.bal = numeric(), 
                                        settlement.date = "character", 
                                        price = numeric(), 
                                        PrepaymentAssumption = "character") 
  {standardGeneric("MortgageCashFlow")})
  
  #' A standard generic function to access the slot Price
  #' @param object an S4 class object
  #' @export
  setGeneric("Price", function(object)
  {standardGeneric("Price")})
  
  #' A standard generic function to access the slot Accrued
  #' @param object an S4 class object
  #' @export
  setGeneric("Accrued", function(object)
  {standardGeneric("Accrued")})
  
  #' A standard generic function to access the slot YieldToMaturity
  #' @param  object an S4 class object
  #' @export
  setGeneric("YieldToMaturity", function(object)
  {standardGeneric("YieldToMaturity")})
  
  #' A standard generic function to access the slot WAL
  #' @param object an S4 class object
  #' @export
  setGeneric("WAL", function(object)
  {standardGeneric("WAL")})
  
  #' A standard generic function to access the slot PrepaidPrin
  #' @param object an object whose method signature is KDS_PassThrough
  #' @export
  setGeneric("PrepaidPrin", function(object)
  {standardGeneric("PrepaidPrin")})
  
  setMethod("initialize",
          signature("MortgageCashFlow"),
          function(.Object,       
                   Price = numeric(),
                   Accrued = numeric(),
                   YieldToMaturity = numeric(),
                   WAL = numeric(),
                   ModDuration = numeric(),
                   Convexity = numeric(),
                   Period = numeric(),
                   PmtDate = "character",
                   TimePeriod = numeric(),
                   BeginningBal = numeric(),
                   MonthlyPmt = numeric(),
                   MonthlyInterest = numeric(),
                   PassThroughInterest = numeric(),
                   ScheduledPrin = numeric(),
                   PrepaidPrin = numeric(),
                   DefaultedPrin = numeric(),
                   LossAmount = numeric(),
                   RecoveredAmount = numeric(),
                   EndingBal = numeric(),
                   ServicingIncome = numeric(),
                   PMIPremium = numeric(),
                   GFeePremium = numeric(),  
                   TotalCashFlow = numeric(),
                   ...){
            callNextMethod(.Object,
                           Price = Price,
                           Accrued = Accrued,
                           YieldToMaturity = YieldToMaturity,
                           WAL = WAL,
                           ModDuration = ModDuration,
                           Convexity = Convexity,
                           Period = Period,
                           PmtDate = PmtDate,
                           TimePeriod = TimePeriod,
                           BeginningBal = BeginningBal,
                           MonthlyPmt = MonthlyPmt,
                           MonthlyInterest = MonthlyInterest,
                           PassThroughInterest = PassThroughInterest,
                           ScheduledPrin = ScheduledPrin,
                           PrepaidPrin = PrepaidPrin,
                           DefaultedPrin = DefaultedPrin,
                           LossAmount = LossAmount,
                           RecoveredAmount = RecoveredAmount,
                           EndingBal = EndingBal,
                           ServicingIncome = ServicingIncome,
                           PMIPremium = PMIPremium,
                           GFeePremium = GFeePremium,
                           TotalCashFlow = TotalCashFlow,
                           ...)
          })

  #' Method to extract Price from S4 class
  #' @param object the name of the S4 object
  #' @exportMethod Price
  setMethod("Price", signature("MortgageCashFlow"),
            function(object){object@Price})
  
  #' Method to extract Accrued from S4 class
  #' @param object the nameof the S4 object
  #' @exportMethod Accrued
  setMethod("Accrued", signature = ("MortgageCashFlow"),
            function(object){object@Accrued})
  
  #' Method to extract YieldToMaturity from S4 class
  #' @param object the name of the S4 object
  #' @exportMethod YieldToMaturity
  setMethod("YieldToMaturity", signature = ("MortgageCashFlow"),
            function(object){object@YieldToMaturity})
  
  #' Method to extract WAL from S4 class
  #' @param object the name of the S4 object
  #' @exportMethod WAL
  setMethod("WAL", signature = ("MortgageCashFlow"),
            function(object){object@WAL})
  
  #' Method to extract Prepaid Principal from class MortgageCashFlow
  #' @param object the name of the object of type MortgageCashFlow
  #' @exportMethod PrepaidPrin
  setMethod("PrepaidPrin", signature("MortgageCashFlow"),
            function(object){object@PrepaidPrin})

  #'  A function to compute the cash flow of a pool of securitized mortgages
  #' 
  #' This is a generic function used to construct the class object MortgageCashFlow.
  #' For this function to work properly the classes MBSDetails and PrepaymentAssumption.
  #' must be present and loaded into the local environment.
  #' @param bond.id A character string referencing the object MBSDetails.
  #' @param original.bal The original balance of the MBS pool.
  #' @param settlement.date The settlment date of the MBS trade.
  #' For example $102 is input as 102.00 not 1.02.
  #' @param price A numeric value the price traded.  Price is input as a whole number.
  #' @param PrepaymentAssumption A character string referencing the class object.
  #' @examples
  #' \dontrun{
  #' MortgageCashFlow(bond.id = "bondlabMBS4", original.bal = 1000000000,
  #' settlement.date = "01-13-2013", price = 104.00, PrepaymentAssumption = "Prepayment")}
  #' @export MortgageCashFlow
  MortgageCashFlow <- function(bond.id = "character", 
                             original.bal = numeric(), 
                             settlement.date = "character",
                             price = numeric(),
                             PrepaymentAssumption = "character"){
  
  #This function error traps mortgage bond inputs
  ErrorTrap(bond.id = bond.id, 
            principal = original.bal, 
            settlement.date = settlement.date, 
            price = price)
  
  issue.date = as.Date(bond.id@IssueDate, "%m-%d-%Y")
  start.date = as.Date(bond.id@DatedDate, "%m-%d-%Y")
  end.date = as.Date(bond.id@Maturity, "%m-%d-%Y")
  lastpmt.date = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  nextpmt.date = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  delay = bond.id@PaymentDelay
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  bondbasis = bond.id@BondBasis
  
  
  #  Validate the price and coupon passed through the error trapping function
  #  This validates that the correct unit is passed into the Bond Cash Flow function
  if(price <= 1) {price = price} else {price = price/100}
  
  # calcualte beginning balance (principal) from the MBS pool factor
  # accrued interest is calculated using the current factor
  factor = bond.id@MBSFactor
  principal = original.bal * factor
  
  # The factor must be adjusted by the prepayment assumption so the 
  # investor estimated cashflow is accurately projected following
  # TBA settlement
  
  if(PrepaymentAssumption@PrepayAssumption == "CPR"){
    AdjFactor = factor - PrepaymentAssumption@SMM[1]
  } else {AdjFactor = factor}
  
  AdjPrincipal = original.bal * AdjFactor
  
  MBS.CF.Table = CashFlowEngine(bond.id = bond.id,
                                settlement.date = settlement.date,
                                principal = AdjPrincipal,
                                PrepaymentAssumption = PrepaymentAssumption)

  #step5 calculate accrued interest for the period
  days.to.nextpmt = (BondBasisConversion(issue.date = issue.date, 
                                         start.date = start.date, 
                                         end.date = end.date, 
                                         settlement.date = settlement.date,
                                         lastpmt.date = lastpmt.date,
                                         nextpmt.date = nextpmt.date,
                                         type = bondbasis)) * days.in.year.360
  
  days.between.pmtdate = ((months.in.year/frequency)/months.in.year) * days.in.year.360
  days.of.accrued = (days.between.pmtdate - days.to.nextpmt) 
  accrued.interest = (days.of.accrued/days.between.pmtdate) * ((bond.id@Coupon/yield.basis)/frequency) * AdjPrincipal
 
  # Step6 solve for yield to maturity given the price of the bond.  irr is an internal function used to solve for yield to maturity
  # it is internal so that the bond's yield to maturity is not passed to a global variable that may inadvertantly use the value

  irr <- function(rate, 
                  time.period, 
                  cashflow, 
                  principal, 
                  price, 
                  accrued.interest){
    pv = cashflow * 1/(1+rate) ^ time.period
    proceeds = principal * price
    sum(pv) - (proceeds + accrued.interest)}
  
  
    # create logic for the time weight between Time (neo) Period (classic)
    ytm = try(uniroot(irr, 
                      interval = c(lower = -.75, upper = .75), 
                      tol = tolerance, 
                      time.period = as.numeric(MBS.CF.Table[,"Time"]), 
                      cashflow = round(as.numeric(MBS.CF.Table[,"Investor CashFlow"]),8), 
                      principal = AdjPrincipal, 
                      price = price, 
                      accrued.interest = accrued.interest)$root)
    
  # Because I use the time weights here the is no need to make adjustment
  # If we use period weights we will need an adjustment  
    Yield.To.Maturity = ytm
  
  #Step7 Present value of the cash flows Present Value Factors
  MBS.CF.Table[,"Present Value Factor"] = 1/((1+(Yield.To.Maturity/frequency))^(MBS.CF.Table[,"Time"] * frequency))
  
  #Present Value of the cash flows
  MBS.CF.Table[,"Present Value"] = MBS.CF.Table[,"Investor CashFlow"] * MBS.CF.Table[,"Present Value Factor"]
  
  #Step8 Risk measures Duration Factors
  MBS.CF.Table[,"Duration"] = MBS.CF.Table[,"Time"] * (MBS.CF.Table[,"Present Value"]/((principal * price) + accrued.interest))
  
  # Weighted Average Life 
  WAL = sum(((MBS.CF.Table[,"Scheduled Prin"] + MBS.CF.Table[,"Prepaid Prin"] + MBS.CF.Table[,"Recovered Amount"]) * MBS.CF.Table[,"Time"])/ 
              sum(MBS.CF.Table[,"Scheduled Prin"] + MBS.CF.Table[,"Prepaid Prin"] + MBS.CF.Table[,"Recovered Amount"]))
  
  #Convexity Factors
  MBS.CF.Table[,"Convexity Time"] = MBS.CF.Table[,"Time"] *(MBS.CF.Table[,"Time"] + 1)
  
  MBS.CF.Table[,"CashFlow Convexity"] = (MBS.CF.Table[,"Investor CashFlow"]/((1 + ((Yield.To.Maturity)/frequency)) ^ ((MBS.CF.Table[,"Time"] + 2) * frequency)))/ 
    ((principal * price) + accrued.interest)
  
  MBS.CF.Table[,"Convexity"] = MBS.CF.Table[,"Convexity Time"] * MBS.CF.Table[,"CashFlow Convexity"] 
  
  
  #Duration and Convexity
  Duration = apply(MBS.CF.Table, 2, sum)["Duration"]
  Modified.Duration = Duration/(1 + (Yield.To.Maturity/frequency))
  Convexity = apply(MBS.CF.Table, 2, sum)["Convexity"] * .5
  
  #Create Class Mortgage Loan Cashflows
  new("MortgageCashFlow",
      Price = price * price.basis,
      Accrued = accrued.interest,
      YieldToMaturity = Yield.To.Maturity,
      WAL = WAL,
      ModDuration = Modified.Duration,
      Convexity = Convexity,
      Period = MBS.CF.Table[,"Period"],
      PmtDate = as.character(as.Date(MBS.CF.Table[,"Date"], origin = "1970-01-01")),
      TimePeriod = MBS.CF.Table[,"Time"],
      BeginningBal = MBS.CF.Table[,"Begin Bal"],
      MonthlyPmt = MBS.CF.Table[,"Monthly Pmt"],
      MonthlyInterest = MBS.CF.Table[,"Scheduled Int"],
      PassThroughInterest = MBS.CF.Table[,"Pass Through Interest"],
      ScheduledPrin = MBS.CF.Table[,"Scheduled Prin"],
      PrepaidPrin = MBS.CF.Table[,"Prepaid Prin"],
      DefaultedPrin = MBS.CF.Table[,"Defaulted Prin"],
      LossAmount = MBS.CF.Table[,"Loss Amount"],
      RecoveredAmount = MBS.CF.Table[,"Recovered Amount"],
      EndingBal = MBS.CF.Table[,"Ending Bal"],
      ServicingIncome = MBS.CF.Table[,"Servicing"],
      PMIPremium = MBS.CF.Table[,"PMI"],    
      GFeePremium = MBS.CF.Table[,"GFee"],
      TotalCashFlow = MBS.CF.Table[,"Investor CashFlow"]
  )}
