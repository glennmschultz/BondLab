
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

  #' An S4 Class a mortgage cashflow array
  #' 
  #' This function is used in MortgageCashFlow_Array to facilitate
  #' passing a cashflow array from an external cash flow engine
  #' 
  #' @slot CashFlowArray an array of mortgage cashflows 
  setClass("MortgageCashFlowArray",
         representation(
           CashFlowArray = "array"),
         prototype = (CashFlowArray = array(numeric(), c(0,0,0))))

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
  #' this function can be useful when using distributed processing and/or 
  #' external
  #' prepayment and interest rate models
  #' @param CashFlowArray an array of cashflows the user would like to pass 
  #' to a function
  #' @export MortgageCashFlowArray 
  MortgageCashFlowArray <- function(CashFlowArray = array()){
    new("MortgageCashFlowArray",
        CashFlowArray = CashFlowArray)
  }