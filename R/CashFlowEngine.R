
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

  #setGeneric("CashFlowEngine",function (bond.id = "character",
  #                                      settlement.date = "character",
  #                                      principal = numeric(), 
  #                                      PrepaymentAssumption = "character")
  #  {standardGeneric("CashFlowEngine")})

  #' Bond Lab function the residential mortgage cash flow engine
  #' 
  #' @param bond.id A character string referring to an object of type MBSDetails
  #' @param settlement.date A character string the settlement date
  #' @param principal A numeric value the principal balance
  #' @param PrepaymentAssumption A character string referencing 
  #' an object type PrepaymentModel
  #' @export
  CashFlowEngine <- function (bond.id = "character",
                              settlement.date = "character",
                              principal = numeric(), 
                              PrepaymentAssumption = "character"){
    
    issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
    start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
    end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
    lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    coupon = Coupon(bond.id) #bond.id@Coupon
    frequency = Frequency(bond.id) #bond.id@Frequency
    delay = PaymentDelay(bond.id) #bond.id@PaymentDelay
    settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
    bondbasis = BondBasis(bond.id) #bond.id@BondBasis
    
    #Mortgage specific inputs
    note.rate = GWac(bond.id)
    FirstPmtDate = FirstPmtDate(bond.id)
    FinalPmtDate = FinalPmtDate(bond.id)
    servicing.fee = Servicing(bond.id)
    pmi = PMI(bond.id)
    g.fee = GFeePremium(bond.id)
  
    #Build a vector of dates for the payment schedule
    # first get the pmtdate interval
    pmtdate.interval = months.in.year/frequency
    
    # then compute the payment dates
    pmtdate = as.Date(c(if(settlement.date == issue.date) 
    {seq(start.date + delay, end.date + delay, 
         by = paste(pmtdate.interval, "months"))
    } else {seq(nextpmt.date + delay, end.date + delay, 
                by = paste(pmtdate.interval, "months"))}), "%m-%d-%Y")
    
    # pass coupon to the coupon types class for conversion to CouponDecimal,
    # CouponBasis, and CouponDecimalString
    Coupon <- CouponTypes(coupon = coupon)
  
    #  Validate the coupon and note rate passed through the error 
    #  trapping function This validates that the correct unit is 
    #  passed into the Bond Cash Flow function
    if(coupon > 1) {coupon = coupon/yield.basis} else {coupon = coupon}
    if(note.rate > 1) {note.rate = note.rate/yield.basis
    } else {note.rate = note.rate}
  
    # Build the time period vector (n) for discounting the cashflows 
    # nextpmt date is vector of payment dates to n for each period.  
    # need TBA versus T + 3 settlement date logic
    # if T + 3 then settlement date is equal to settlement date
    time.period = BondBasisConversion(issue.date = issue.date,
                                      start.date = start.date,
                                      end.date = end.date,
                                      settlement.date = settlement.date,
                                      lastpmt.date = lastpmt.date,
                                      nextpmt.date = pmtdate,
                                      type = bondbasis)
    
    # step5 calculate accrued interest for the period this is also
    # the remain time period used to adjust the period vector 
    days.to.nextpmt = (BondBasisConversion(
      issue.date = issue.date,
      start.date = start.date,
      end.date = end.date,
      settlement.date = settlement.date,
      lastpmt.date = lastpmt.date,
      nextpmt.date = nextpmt.date,
      type = bondbasis)) * days.in.year.360
    
    days.between.pmtdate = ((months.in.year/frequency)/months.in.year) * 
      days.in.year.360
    
    days.of.accrued = (days.between.pmtdate - days.to.nextpmt)
    remain.period = days.of.accrued/days.in.year.360
    
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
    MBS.CF.Table[x,"Date"] = pmtdate[x]
    MBS.CF.Table[x,"Time"] = time.period[x]
    if (MBS.CF.Table[x,"Period"] == 1) {MBS.CF.Table[x,"Begin Bal"] = principal
    } else {MBS.CF.Table[x,"Begin Bal"] = MBS.CF.Table[x-1,"Ending Bal"]}
    MBS.CF.Table[x,"Monthly Pmt"] = 
      Mortgage.Monthly.Payment(orig.bal = MBS.CF.Table[x,"Begin Bal"],
              note.rate = note.rate,
              term.mos = (num.periods - MBS.CF.Table[x,"Period"] + 1))
    MBS.CF.Table[x,"Scheduled Int"] = 
      MBS.CF.Table[x,"Begin Bal"] * (note.rate/months.in.year)
    MBS.CF.Table[x,"Scheduled Prin"] =
      Sched.Prin(balance = MBS.CF.Table[x,"Begin Bal"],
                 note.rate = note.rate,
                 term.mos = (num.periods - MBS.CF.Table[x,"Period"] + 1), 
                 period = 1)
    if(x != num.periods) {MBS.CF.Table[x,"Prepaid Prin"] = 
      PrepaymentAssumption@SMM[x] * (MBS.CF.Table[x,"Begin Bal"] - 
                                       MBS.CF.Table[x,"Scheduled Prin"])
    } else {MBS.CF.Table[x,"Prepaid Prin"] = 0}
      
    if(x!= num.periods) {MBS.CF.Table[x,"Defaulted Prin"] = 
      PrepaymentAssumption@MDR[x] * MBS.CF.Table[x,"Begin Bal"]
      } else {MBS.CF.Table[x,"Defaulted Prin"] = 0}
      
    MBS.CF.Table[x,"Loss Amount"] = MBS.CF.Table[x,"Defaulted Prin"] *
      PrepaymentAssumption@Severity[x]
      
    MBS.CF.Table[x,"Recovered Amount"] = MBS.CF.Table[x,"Defaulted Prin"] -
      MBS.CF.Table[x,"Loss Amount"]
    
   MBS.CF.Table[x,"Ending Bal"] = MBS.CF.Table[x,"Begin Bal"] -
     (MBS.CF.Table[x,"Scheduled Prin"] + MBS.CF.Table[x,"Prepaid Prin"] +
        MBS.CF.Table[x,"Defaulted Prin"])
  MBS.CF.Table[x,"Servicing"] = MBS.CF.Table[x,"Begin Bal"] *
    (servicing.fee/monthly.yield.basis)
  MBS.CF.Table[x,"PMI"] = MBS.CF.Table[x,"Begin Bal"] * 
    (pmi/monthly.yield.basis)
  MBS.CF.Table[x,"GFee"] = MBS.CF.Table[x,"Begin Bal"] * 
    (g.fee/monthly.yield.basis)
  MBS.CF.Table[x,"Pass Through Interest"] = MBS.CF.Table[x,"Begin Bal"] * 
    (CouponBasis(Coupon)/months.in.year)
  MBS.CF.Table[x,"Investor CashFlow"] = MBS.CF.Table[x,"Scheduled Prin"] + 
    MBS.CF.Table[x,"Prepaid Prin"] + 
  MBS.CF.Table[x,"Recovered Amount"] + MBS.CF.Table[x,"Pass Through Interest"]
  }
    return(MBS.CF.Table)
}

