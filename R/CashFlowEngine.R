  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # File License
  # Copyright (C) 2015 Bond Lab Technologies, Inc

  setGeneric("CashFlowEngine",function (bond.id = "character",
                                        settlement.date = "character",
                                        principal = numeric(), 
                                        PrepaymentAssumption = "character")
    {standardGeneric("CashFlowEngine")})

  #' Bond Lab function the residential mortgage cash flow engine
  #' 
  #' @param bond.id A character string referring to an object of type MBSDetails
  #' @param settlement.date A character string the settlement date
  #' @param principal A numeric value the principal balance
  #' @param PrepaymentAssumption A character string referencing 
  #' an object type PrepaymentAssumption

  CashFlowEngine <- function (bond.id = "character",
                              settlement.date = "character",
                              principal = numeric(), 
                              PrepaymentAssumption = "character"){
    
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
    
    #Mortgage specific inputs
    note.rate = bond.id@GWac
    FirstPmtDate = bond.id@FirstPmtDate
    FinalPmtDate = bond.id@FinalPmtDate
    servicing.fee = bond.id@Servicing
    pmi = bond.id@PMI
    g.fee = bond.id@Gfee
  
    #Build a vector of dates for the payment schedule
    # first get the pmtdate interval
    pmtdate.interval = months.in.year/frequency
    # then compute the payment dates
    pmtdate = as.Date(c(if(settlement.date == issue.date) 
    {seq(start.date, end.date, by = paste(pmtdate.interval, "months"))
    } else {seq(nextpmt.date, end.date, by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y")
  
    #  Validate the coupon and note rate passed through the error trapping function
    #  This validates that the correct unit is passed into the Bond Cash Flow function
    if(coupon > 1) {coupon = coupon/100} else {coupon = coupon}
    if(note.rate > 1) {note.rate = note.rate/100} else {note.rate = note.rate}
    
    #Build the time period vector (n) for discounting the cashflows 
    #nextpmt date is vector of payment dates to n for each period.  The differnce between
    
    time.period = BondBasisConversion(issue.date = issue.date, 
                                    start.date = start.date, 
                                    end.date = end.date, 
                                    settlement.date = settlement.date,
                                    lastpmt.date = lastpmt.date, 
                                    nextpmt.date = pmtdate,
                                    type = bondbasis)
    
    #Count the number of cashflows 
    #num.periods is the total number of cashflows to be received
    #num.period is the period in which the cashflow is received
    num.periods = length(time.period)
    #step5 initialize the prepayment model assumption class
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
  
    MBS.CF.Table <- array(data = NA, 
                          c(num.periods, 23), 
                          dimnames = list(seq(c(1:num.periods)),col.names))  
  
    for(x in 1:num.periods){
      MBS.CF.Table[x,"Period"] = x
      MBS.CF.Table[x,"Date"] = pmtdate[x] + delay
      MBS.CF.Table[x,"Time"] = time.period[x]
      if (MBS.CF.Table[x,"Period"] == 1) {MBS.CF.Table[x,"Begin Bal"] = principal
      } else {MBS.CF.Table[x,"Begin Bal"] = MBS.CF.Table[x-1,"Ending Bal"]}

      MBS.CF.Table[x,"Monthly Pmt"] = Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x,"Begin Bal"], 
                                                               note.rate = note.rate, term.mos = (num.periods - MBS.CF.Table[x,"Period"] + 1))
      MBS.CF.Table[x,"Scheduled Int"] = MBS.CF.Table[x,"Begin Bal"] * (note.rate/months.in.year)
      MBS.CF.Table[x,"Scheduled Prin"] = Sched.Prin(balance = MBS.CF.Table[x,"Begin Bal"], note.rate = note.rate, 
                                                    term.mos = (num.periods - MBS.CF.Table[x,"Period"] + 1), period = 1)
      
      if(x != num.periods) {MBS.CF.Table[x,"Prepaid Prin"] = PrepaymentAssumption@SMM[x] * (MBS.CF.Table[x,"Begin Bal"] - MBS.CF.Table[x,"Scheduled Prin"])
      } else {MBS.CF.Table[x,"Prepaid Prin"] = 0}
      
      if(x!= num.periods) {MBS.CF.Table[x,"Defaulted Prin"] = PrepaymentAssumption@MDR[x] * MBS.CF.Table[x,"Begin Bal"]
      } else {MBS.CF.Table[x,"Defaulted Prin"] = 0}
      
      MBS.CF.Table[x,"Loss Amount"] = MBS.CF.Table[x,"Defaulted Prin"] * PrepaymentAssumption@Severity[x]
      
      MBS.CF.Table[x,"Recovered Amount"] = MBS.CF.Table[x,"Defaulted Prin"] - MBS.CF.Table[x,"Loss Amount"]
      
      MBS.CF.Table[x,"Ending Bal"] = MBS.CF.Table[x,"Begin Bal"] - (MBS.CF.Table[x,"Scheduled Prin"] + MBS.CF.Table[x,"Prepaid Prin"] + MBS.CF.Table[x,"Defaulted Prin"])
      MBS.CF.Table[x,"Servicing"] = MBS.CF.Table[x,"Begin Bal"] * (servicing.fee/monthly.yield.basis)
      MBS.CF.Table[x,"PMI"] = MBS.CF.Table[x,"Begin Bal"] * (pmi/monthly.yield.basis)
      MBS.CF.Table[x,"GFee"] = MBS.CF.Table[x,"Begin Bal"] * (g.fee/monthly.yield.basis)
      MBS.CF.Table[x,"Pass Through Interest"] = MBS.CF.Table[x,"Begin Bal"] * (coupon/months.in.year)
      MBS.CF.Table[x,"Investor CashFlow"] = MBS.CF.Table[x,"Scheduled Prin"] + MBS.CF.Table[x,"Prepaid Prin"] + 
      MBS.CF.Table[x,"Recovered Amount"] + MBS.CF.Table[x,"Pass Through Interest"]
    }
    
    
    return(MBS.CF.Table)
}

