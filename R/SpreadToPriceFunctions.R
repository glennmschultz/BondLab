
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
  # A function differencing the proceeds and present value used with
  # uni-root to find the spot spread to normalize discounting
  
  FindSpotSpread <- function(spot.spread = vector(),
                             spot.curve = vector(),
                             cash.flow = vector(),
                             time.period = vector(),
                             proceeds = numeric()){
  
    DiscRate = spot.spread + spot.curve
    Present.Value = sum((1/(1+(DiscRate)) ^ time.period) * cash.flow)
    return(proceeds - Present.Value)
  }
  
  #' A function to solve for the spread to the spot rate curve
  #'
  #' @param spot.curve A vector representing the spot rate curve
  #' @param cash.flow A vector representing the cash flow paid to the
  #' investor
  #' @param time.period A vector representing the time weights used to
  #' discount cash flow
  #' @param proceeds A numeric value proceeds paid by the investor
  SolveSpotSpread <- function(spot.curve = vector(),
                              cash.flow = vector(),
                              time.period = vector(),
                              proceeds = numeric()){
    
    spot.spread = uniroot(FindSpotSpread,
                          interval = c(-.2, .2),
                          tol = tolerance,
                          spot.curve = spot.curve,
                          cash.flow = cash.flow,
                          time.period = time.period,
                          proceeds = proceeds)$root
    return(spot.spread)}
  
  #' A function to solve for the price of a MBS pass-through given
  #' both pricing speed and spread to the interpolated curve.  The function
  #' returns the clean price of the bond
  #' @param bond.id A character string the cusip number or bond.id
  #' @param trade.date A character string the trade date mm-dd-YYYY
  #' @param settlement.date A character string the settlement date mm-dd-YYYY
  #' @param PrepaymentAssumption the assumption must be: "CPR", "PPC", "MODEL"
  #' @param spread a numeric value the spread to the interpolated curve
  #' entered in percent
  #' @param CPR a numeric value the CPR assumption used to price the MBS
  #' e.g. 100 basis points = 1
  #' @export
  SpreadToPrice <- function(bond.id = "character",
                            trade.date = "character",
                            settlement.date = "character",
                            PrepaymentAssumption = "character",
                            spread = numeric(),
                            CPR = numeric()){
    
    rates.data <- Rates(trade.date = trade.date)
    bond.id = MBS(MBS.id = bond.id)
    MortgageRate = MtgRate()
    Burnout = BurnOut(bond.id)
    principal = orig.bal * MBSFactor(bond.id)
    frequency = Frequency(bond.id)
    orig.bal = OriginalBal(bond.id)
    
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
    
    TermStructure <- TermStructure(rates.data = rates.data)
    
    prepayment = PrepaymentModel(
      bond.id = bond.id,
      TermStructure = TermStructure,
      MortgageRate = MortgageRate,
      ModelTune = ModelTune,
      Burnout = Burnout,
      PrepaymentAssumption = PrepaymentAssumption,
      CPR = CPR)
    
    # local regression smooth of market curve
    ModelCurve <- loess(as.numeric(rates.data[1,2:12]) ~
                          as.numeric(rates.data[2,2:12]),
                        data = rates.data)
    
    MBS.CF.Table = CashFlowEngine(
      bond.id = bond.id,
      settlement.date = settlement.date,
      principal = principal,
      PrepaymentAssumption = prepayment)
    
    #step5 calculate accrued interest for the period
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
    accrued.interest = (days.of.accrued/days.between.pmtdate) *
      as.numeric(MBS.CF.Table[1,"Pass Through Interest"])
    
    # Weighted Average Life
    WAL = sum(((MBS.CF.Table[,"Scheduled Prin"] +
                  MBS.CF.Table[,"Prepaid Prin"] +
                  MBS.CF.Table[,"Recovered Amount"]) *
                 MBS.CF.Table[,"Time"])/
                sum(MBS.CF.Table[,"Scheduled Prin"] +
                      MBS.CF.Table[,"Prepaid Prin"] +
                      MBS.CF.Table[,"Recovered Amount"]))
    
    # use predict ModelCurve to determine
    ICurve = predict(ModelCurve, WAL)
    YieldTypes <- YieldTypes( yield = (ICurve + spread))
    
    # Present value of the cash flows Present Value Factors
    MBS.CF.Table[,"Present Value Factor"] =
      1/((1+(YieldBasis(YieldTypes)/frequency))^(MBS.CF.Table[,"Time"] *
                                              frequency))
    
    # Present Value of the cash flows
    MBS.CF.Table[,"Present Value"] =
      MBS.CF.Table[,"Investor CashFlow"] *
      MBS.CF.Table[,"Present Value Factor"]
    
    price = ((sum(MBS.CF.Table[,"Present Value"]) - accrued.interest) /
               principal) * price.basis
    
    return(price)
  }
  
  
  
  
  