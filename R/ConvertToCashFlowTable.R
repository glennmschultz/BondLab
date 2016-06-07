

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.



  setAs(from = "MortgageCashFlow", to = "array", def = function(from,to)
  cbind(from@Period,
        from@PmtDate,
        from@TimePeriod,
        from@BeginningBal,
        from@MonthlyPmt,
        from@MonthlyInterest,
        from@PassThroughInterest,
        from@ScheduledPrin,
        from@PrepaidPrin,
        from@DefaultedPrin,
        from@LossAmount,
        from@RecoveredAmount,
        from@EndingBal,
        from@ServicingIncome,
        from@PMIPremium,
        from@GFeePremium,
        from@TotalCashFlow))

  #' A function to convert an S4 MortgaageCashFlow class to an array
  #' 
  #' The function is used as part of the structuring waterfall script
  #' to convert the MortgageCashFlow class to array to facilitate the division
  #' of principal and interest
  #' @param CashFlow A character string referring to an object of type MortgageCashFlow
  #' @export CashFlowTable
  CashFlowTable <- function(CashFlow = "character"){
    
    col.names <- c("Period", 
                   "Date", 
                   "Time", 
                   "Begin Bal", 
                   "Monthly Pmt", 
                   "Scheduled Int",
                   "Pass Through Interest",
                   "Scheduled Prin", 
                   "Prepaid Prin",
                   "Defaulted Prin",
                   "Loss Amount",
                   "Recovered Amount",
                   "Ending Bal", 
                   "Servicing", 
                   "PMI", 
                   "GFee", 
                   "CashFlow")
    
    CFT <- as(CashFlow, "array")
    colnames(CFT) <- col.names
    return(CFT)

  }
   