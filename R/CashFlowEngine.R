
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
    coupon = Coupon(bond.id)
    frequency = Frequency(bond.id)
    delay = PaymentDelay(bond.id)
    settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
    bondbasis = BondBasis(bond.id)
    
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
  
  #'@title Bond Lab function to cash flow a bond
  #'@description calculates the bond cashflow table used in function BondCashflows.
  #'@param bond.id a character or connection referring to an object of type BondDetails.
  #'@param principal the investor principal amount.
  #'@param settlement.date a character the settlement data mm-dd-YYYY.
  #'@export CashFlowBond
  CashFlowBond <- function(bond.id,
                           principal,
                           settlement.date){
    issue.date = as.Date(IssueDate(bond.id), "%m-%d-%Y")
    start.date = as.Date(DatedDate(bond.id), "%m-%d-%Y")
    end.date = as.Date(Maturity(bond.id), "%m-%d-%Y")
    lastpmt.date = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    nextpmt.date = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    coupon = Coupon(bond.id)
    frequency = Frequency(bond.id)       
    settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
    bondbasis = BondBasis(bond.id)
    
    # Test payment dates against settlement dates and roll forward if 
    # settlement date crossses over the payment date
    if(as.Date(settlement.date, format = '%m-%d-%Y') >=
       as.Date(NextPmtDate(bond.id), format = '%m-%d-%Y')){
      bond.id <- `LastPmtDate<-`(bond.id, NextPmtDate(bond.id))}
    
    if(as.Date(LastPmtDate(bond.id), format = '%m-%d-%Y') 
       == as.Date(NextPmtDate(bond.id), format = '%m-%d-%Y')){
      bond.id <- `NextPmtDate<-`(bond.id,
                                 as.character(format(
                                   as.Date(LastPmtDate(bond.id), format = "%m-%d-%Y") %m+% 
                                     months(months.in.year/Frequency(bond.id)), 
                                   "%m-%d-%Y")))}
    
    Coupon <- CouponTypes(coupon = coupon)
    
    leap_day <- function(pmt.date){
      for(date in seq_along(pmt.date)){
        if(leap_year(pmt.date)[date] == TRUE & 
           month(pmt.date)[date] == 2 & day(pmt.date)[date] == 28){
          pmt.date[date] = pmt.date[date] %m+% days(1)} else {next}
      }
      return(pmt.date)
    }
    
    # Calculate the number of cashflows that will be paid from settlement date to
    # maturity date 
    # step1 calculate the years to maturity
    #ncashflows = (as.numeric(
    #  difftime(end.date, nextpmt.date, units = 'days'))/days.in.month
    #  ) %/% (months.in.year/frequency) + 1
    
    ncashflows = max(BondBasisConversion(issue.date = issue.date,
                            start.date = start.date,
                            end.date = end.date,
                            settlement.date = settlement.date,
                            lastpmt.date = start.date,
                            nextpmt.date = end.date,
                            type = bondbasis) %/% (1/frequency),1)
    
    monthvector <- seq(1, ncashflows,1) * (months.in.year/frequency)
    pmt.date = c(as.Date(DatedDate(bond.id), format = '%m-%d-%Y') %m+% months(monthvector))
    pmt.date <- leap_day(pmt.date)
    
    #get the index number of the last payment date made to the investor
    pmtindex = if(any(pmt.date == as.Date(LastPmtDate(bond.id),format ='%m-%d-%Y')) == FALSE) {1
      } else {which(pmt.date == as.Date(LastPmtDate(bond.id),format ='%m-%d-%Y')) + 1}
    pmtdate = pmt.date[pmtindex:length(pmt.date)]
    
    
    #ncashflows = max(BondBasisConversion(
    #  issue.date = issue.date, 
    #  start.date = start.date, 
    #  end.date = end.date, 
    #  settlement.date = lastpmt.date,
    #  lastpmt.date = lastpmt.date, 
    #  nextpmt.date = end.date, 
    #  type = bondbasis) %/% (1/frequency),1)
    
    # then compute the payment dates based on payment date interval.  The logic
    # including the settlement.date == issue.date is to facilitate adding first
    # payment date to BondDetails object.  Due to differing bond basis the payment
    # date vector must be calculated
    
    #if(ncashflows == 1 & (settlement.date < end.date)) {pmtdate = end.date
    #} else pmtdate = c(as.Date(LastPmtDate(bond.id), format = '%m-%d-%Y') %m+% months(monthvector))
    
    # in leap years, notably UST, even though bond basis is actual/365 the maturity
    # date reflect 366 days if the bond matures in Feb. leap year.  Thus, to ensure the 
    # correct maturity date the last payment in the vector is checked against the maturity
    # date.
    numpayments = length(pmtdate)
    #monthvector[numpayments] == end.date

    #step3 build the time period vector (n) for discounting the cashflows 
    #nextpmt date is vector of payment dates to n for each period

    time.period <- numeric(numpayments)
    for(pmt in seq_along(pmtdate))
      time.period[pmt] = BondBasisConversion(
        issue.date = issue.date, 
        start.date = settlement.date, 
        end.date = end.date, 
        settlement.date = settlement.date,
        lastpmt.date = lastpmt.date, 
        nextpmt.date = pmtdate[pmt], 
        type = bondbasis)
    
    #step4 Count the number of cashflows 
    #num.periods is the total number of cashflows to be received
    #num.period is the period in which the cashflow is received
    num.periods = length(time.period)
    col.names <- c("Period",                   #1
                   "Date",                     #2
                   "Time",                     #3
                   "Principal Outstanding",    #4
                   "Coupon",                   #5
                   "Coupon Income",            #6
                   "Principal Paid",           #7
                   "TotalCashFlow",            #8
                   "Present Value Factor",     #9
                   "Present Value",            #10
                   "Duration",                 #11
                   "Convexity Time",           #12
                   "CashFlow Convexity",       #13
                   "Convexity")                #14
    num.columns <- length(col.names)
    Bond.CF.Table <- array(data = NA, c(num.periods, num.columns), 
                           dimnames = list(seq(c(1:num.periods)),col.names))  
    for(i in 1:num.periods){
      Bond.CF.Table[i,"Period"] = i
      Bond.CF.Table[i,"Date"] = pmtdate[i]
      Bond.CF.Table[i,"Time"] = time.period[i]
      Bond.CF.Table[i,"Principal Outstanding"] = principal
      Bond.CF.Table[i,"Coupon"] = CouponBasis(Coupon)
      Bond.CF.Table[i,"Coupon Income"] = 
        Bond.CF.Table[i,"Coupon"] * 
        BondBasisConversion(issue.date = issue.date,
                            #this will need more logic for other than actual
                            start.date = as.Date(ifelse(i == 1, lastpmt.date,
                                                        as.Date(Bond.CF.Table[i-1, 'Date'], 
                                                                origin = "1970-01-01")),
                                                 origin = '1970-01-01'),
                            end.date = end.date,
                            settlement.date = as.Date(ifelse(i == 1, lastpmt.date,
                            as.Date(Bond.CF.Table[i-1, 'Date'], origin = "1970-01-01")),
                            origin = '1970-01-01'),
                            lastpmt.date = as.Date(ifelse(i == 1, lastpmt.date, 
                            as.Date(Bond.CF.Table[i-1, 'Date'], origin = "1970-01-01")),
                            origin = '1970-01-01'),
                            nextpmt.date = as.Date(ifelse(i == 1, nextpmt.date, 
                            as.Date(Bond.CF.Table[i, 'Date'], origin = "1970-01-01")),
                            origin = '1970-01-01'),
                            type = bondbasis) *
        Bond.CF.Table[i,"Principal Outstanding"]
      if(as.Date(Bond.CF.Table[i,"Date"], origin = "1970-01-01") == end.date) {
        Bond.CF.Table[i,"Principal Paid"] = principal
      } else {Bond.CF.Table[i,"Principal Paid"] = 0}
      Bond.CF.Table[i,"TotalCashFlow"] = 
        Bond.CF.Table[i,"Coupon Income"] + Bond.CF.Table[i,"Principal Paid"]
    }
    return(Bond.CF.Table)
    
  }

