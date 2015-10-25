# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014 Bond Lab Technologies, Inc

  setGeneric("MortgageCashFlowArray",
           function(CashFlowArray = array())
             {standardGeneric("MortgageCashFlowArray")})
  
  setMethod("initialize",
            signature("MortgageCashFlowArray"),
            function(.Object,
                    CashFlowArray = array()){
              
              .Object@CashFlowArray = CashFlowArray
              
              return(.Object)
            })
  #' A function to pass a mortgage cashflow array to computation engine
  #' 
  #' The MortgageCashFlowArray function can be used to pass a cashflow
  #' array to a computation engine by passing the internal cashflow engines
  #' this function can be useful when using distributed processing and/or external
  #' prepayment and interest rate models
  #' @param CashFlowArray an array of cashflows the user would like to pass to a function
  #' @export MortgageCashFlowArray 
  MortgageCashFlowArray <- function(CashFlowArray = array()){
    new("MortgageCashFlowArray",
        CashFlowArray = CashFlowArray)
  }