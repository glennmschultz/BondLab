  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.
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
  
  #' @include BondDetails.R MortgageCashFlow.R BondCashFlows.R
  NULL
  
  #' An S4 class representing standared bill cash flows
  #' 
  #' @slot Price a character the price of the bill
  #' @slot Discount a numeric value the discount rate
  #' @slot YieldToMaturity a numeric value the bill yield expressed as semi-annual
  #' bond equivalent
  #' @slot WAL a numeric value the weighted average life of the bond
  #' @slot ModDuration a numeric value the bill modified duration
  #' @slot Convexity a numeric value the bill convexity
  #' @slot Period a numeric value the index of the payment to the investor
  #' @slot PmtDate a character string the payment date to the investor the format
  #' is mm-dd-YYYY.
  #' @slot TimePeriod a numeric value the time period between payment dates made 
  #' to the investor.
  #' @slot PrincipalOutstanding a numeric value the outstanding principal balance
  #' @slot CouponPmt a numeric value the outstanding principal balance
  #' @slot TotalCashFlow a numeric value the sum of the principal and interest 
  #' payment made in each period.
  #' @exportClass BillCashFlows
  setClass("BillCashFlows",
           representation(
             Price = "character",
             Discount = "numeric",
             YieldToMaturity = "numeric",
             WAL = "numeric",
             ModDuration = "numeric",
             Convexity = "numeric",
             Period = "numeric",
             PmtDate = "character",
             TimePeriod = "numeric",
             PrincipalOutstanding = "numeric",
             CouponPmt ="numeric",
             TotalCashFlow ="numeric"
           ))
  
  # Note: standard generic Price is defined in MortgageCashFlow.R
  
  #' A standard generic to get the slot Discount 
  #' @param object an S4 class object
  #' @export Discount
  setGeneric("Discount", function(object)
    {standardGeneric("Discount")})
  
  # Note: standard generic YieldToMaturity is defined in MortgageCashFlow.R
  # Note: standard generic WAL is defined in MortgageCashFlow.R
  # Note: standard generic ModDuration is defined in MortgageCashFlow.R
  # Note: standard generic Convexity is defined in MortgageCashFlow.R
  # Note: standard generic Period is defined in MortgageCashFlow.R
  # Note: standard generic PmtDate is defined in MortgageCashFlow.R
  # Note: standard generic TimePeriod is defined in MortgageCashFlow.R
  # Note: standard generic CouponPmt is defined in BondCashFlow.R
  # Note: standard generic PrincipalOutstanding is defined in BondCashFlow.R
  # Note: standard generic TotalCashFlow is defined in MortgageCashFlow.R
  
  setMethod("initialize",
            signature("BillCashFlows"),
            function(.Object,
                     Price = "character",
                     Discount = "numeric",
                     YieldToMaturity = "numeric",
                     WAL = "numeric",
                     ModDuration = "numeric",
                     Convexity = "numeric",
                     Period = "numeric",
                     PmtDate = "character",
                     TimePeriod = "numeric",
                     PrincipalOutstanding = "numeric",
                     CouponPmt = "numeric",
                     TotalCashFlow = "numeric",
                     ...){
              callNextMethod(.Object,
                             Price = Price,
                             Discount = Discount,
                             YieldToMaturity = YieldToMaturity,
                             WAL = WAL,
                             ModDuration = ModDuration,
                             Convexity = Convexity,
                             Period = Period,
                             PmtDate = PmtDate,
                             TimePeriod = TimePeriod,
                             PrincipalOutstanding = PrincipalOutstanding,
                             CouponPmt = CouponPmt,
                             TotalCashFlow = TotalCashFlow,
                             ...)
                     })
  
  #' Method to get price from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod Price
  setMethod("Price", signature("BillCashFlows"),
            function(object){object@Price})
  
  #' Method to get discount from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod Discount
  setMethod("Discount", signature("BillCashFlows"),
            function(object){object@Discount})
  
  #' Method to get YieldToMaturity from S4 class
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod YieldToMaturity
  setMethod("YieldToMaturity", signature("BillCashFlows"),
            function(object){object@YieldToMaturity})
  
  #' Method to get WAL from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod WAL
  setMethod("WAL", signature("BillCashFlows"),
            function(object){object@YieldToMaturity})
  
  #' Method to get modified duration from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod ModDuration
  setMethod("ModDuration", signature("BillCashFlows"),
            function(object){object@ModDuration})
  
  #' Method to get Convexity from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod Convexity
  setMethod("Convexity", signature("BillCashFlows"),
            function(object){object@Convexity})
  
  #' Method to get Period from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod Period
  setMethod("Period", signature("BillCashFlows"),
            function(object){object@Period})
  
  #' Method to get PmtDate from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod PmtDate
  setMethod("PmtDate", signature("BillCashFlows"),
            function(object){object@PmtDate})
  
  #' Method to get TimePeriod from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object BillCashFlows
  #' @exportMethod TimePeriod
  setMethod("TimePeriod", signature("BillCashFlows"),
            function(object){object@TimePeriod})
  
  #' Method to get Principal outstanding from S4 class BillCashFlows
  #' 
  #' @param object the name of the S$ object BillCashFlows
  #' @exportMethod PrincipalOutstanding
  setMethod("PrincipalOutstanding", signature("BillCashFlows"),
            function(object){object@PrincipalOutstanding})
  
  #' Method to get TotalCashFlows from S4 class BillCashFlows
  #' 
  #' @param object the name of the S4 object
  #' @exportMethod TotalCashFlow
  setMethod("TotalCashFlow", signature("BillCashFlows"),
            function(object){object@TotalCashFlow})

  
  #' Bill Cash Flow engine for a standard discount bill
  #' 
  #' @param bill.id A character the referencing an object of type BillDetails
  #' @param principal A numeric value the principal or face amount of the bill
  #' @param settlement.date A character the settlment date mm-dd-YYYY
  #' @param price A character the price of the bill
  #' @export BillCashFlows
  BillCashFlows <- function(bill.id,
                            principal,
                            settlement.date,
                            price){
    
    issue.date = as.Date(IssueDate(bill.id), format = '%m-%d-%Y')
    start.date = as.Date(DatedDate(bill.id), format = '%m-%d-%Y')
    end.date = as.Date(Maturity(bill.id), format = '%m-%d-%Y')
    bond.basis = BondBasis(bill.id)
    
    
    # Pass price to the PriceTypes constructor function.  This function allows
    # converts from 32nds and to decimal basis
    price <- PriceTypes(price = price)
    
    
    Bill.CF.Table <- CashFlowBill(bill.id = bill.id,
                                  principal = principal,
                                  settlement.date = settlement.date)
    
    Yield.To.Maturity = BillPriceToYield(bill.id,
                                         price = PriceDecimalString(price),
                                         day.count = 360,
                                         settlement.date = settlement.date)

    
    # pass Yield.To.Maturity to class YieldTypes for conversion to YieldDecimal,
    # YieldBasis, and YieldDecimalString
    Yield <- YieldTypes(yield = Yield.To.Maturity)
    
    #Step7 Present value of the cash flows Present Value Factors
    Bill.CF.Table[,"Present Value Factor"] = 1/((1+(YieldBasis(Yield)))^(Bill.CF.Table[,"Time"]))
    
    #Present Value of the cash flows
    Bill.CF.Table[,"Present Value"] = Bill.CF.Table[,"TotalCashFlow"] * Bill.CF.Table[,"Present Value Factor"]
    
    #Step8 Risk measures Duration Factors
    Bill.CF.Table[,"Duration"] = Bill.CF.Table[,"Time"] * (Bill.CF.Table[,"Present Value"]/((principal * PriceBasis(price))))
    
    #Convexity Factors
    Bill.CF.Table[,"Convexity Time"] = Bill.CF.Table[,"Time"] *(Bill.CF.Table[,"Time"] + 1)
    
    Bill.CF.Table[,"CashFlow Convexity"] = (Bill.CF.Table[,"TotalCashFlow"]/((1 + ((YieldBasis(Yield)))) ^ ((Bill.CF.Table[,"Time"] + 2))))/((principal * PriceBasis(price)))
    
    Bill.CF.Table[,"Convexity"] = Bill.CF.Table[,"Convexity Time"] * Bill.CF.Table[,"CashFlow Convexity"] 
    
    #Weighted Average Life
    WAL = sum((Bill.CF.Table[,"Principal Paid"] * Bill.CF.Table[,"Time"]))/sum(Bill.CF.Table[,"Principal Paid"])
    
    #Duration and Convexity
    Duration = apply(Bill.CF.Table, 2, sum)["Duration"]
    Modified.Duration = Duration/(1 + (YieldBasis(Yield)))
    Convexity = apply(Bill.CF.Table, 2, sum)["Convexity"] * .5
    
    #Assign Values to the slots
    new("BillCashFlows",   
        Price = PriceDecimalString(price),
        Discount = 100 - PriceDecimal(price),
        YieldToMaturity = YieldDecimal(Yield),
        WAL = WAL,
        ModDuration = unname(Modified.Duration),
        Convexity = unname(Convexity),
        Period = unname(Bill.CF.Table[,"Period"]),
        PmtDate = unname(as.character(as.Date(Bill.CF.Table[,"Date"], origin = "1970-01-01"))),
        TimePeriod = unname(Bill.CF.Table[,"Time"]),
        PrincipalOutstanding  = unname(Bill.CF.Table[,"Principal Outstanding"]),
        CouponPmt = unname(Bill.CF.Table[,"Coupon Income"]),
        TotalCashFlow = unname(Bill.CF.Table[,"TotalCashFlow"])
    )
  }