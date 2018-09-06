
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  #' An S4 class to hold CPRLife value
  #' 
  #' The CPRLife value is the CPR which equates the MBS yield to maturity to that
  #' of the prepayment model
  #' @slot CPRLife The CPR which ties the MBS yield to maturity to that of the 
  #' prepayment vector yield to maturity
  #' @exportClass ModelToCPR
  setClass("ModelToCPR",
           representation(
             CPRLife = "numeric")
           )


  #' A standard generic to access the CPRLife slot of class ModelToCPR
  #'
  #' @param object An S4 object class
  #' @export CPRLife
  setGeneric("CPRLife", function(object)
    {standardGeneric("CPRLife")})

  setMethod("initialize",
            signature("ModelToCPR"),
            function(.Object,
                     CPRLife = numeric(),
                     ...){
            callNextMethod(.Object,
                           CPRLife = CPRLife,
                           ...)
        })

  #' A method to extract Life CPR from S4 class
  #' @param object An object of type ModelToCPR
  #' @exportMethod CPRLife
  setMethod("CPRLife", signature("ModelToCPR"),
            function(object){object@CPRLife})

  #' Converts a mortgage prepayment model vector to a CPR measure
  #'
  #' The function ties the model vector yield to an equivalent CPR yield using
  #' secant method of interpolation.
  #' @param bond.id A character string referring to an object of type
  #' MBSDetails.
  #' @param settlement.date A character string the settlement date.  
  #' @param term.structure A character string referring to an object of type
  #' @param original.bal A numeric value the original balance.
  #' @param price A character string the price.
  #' @param yield A numeric value the yield to maturity given a prepayment
  #' model vector
  #' @export ModelToCPR
  ModelToCPR <- function(bond.id,
                         settlement.date,
                         term.structure,
                         original.bal,
                         price,
                         yield){

    price <- PriceTypes(price)
    
    yieldcpr <- function(cpr,
                        bond.id,
                        original.bal,
                        settlement.date,
                        term.structure,
                        price,
                        yield){
      
      price <- PriceTypes(price)
      
     prepayment.assumption <- PrepaymentModel(bond.id = bond.id, 
                                              term.structure = term.structure, 
                                              prepayment.assumption = 'CPR', 
                                              cpr = cpr)
            
    mtg.cashflow <- MortgageCashFlow(bond.id = bond.id,
                                     original.bal = original.bal,
                                     settlement.date = settlement.date,
                                     price = PriceDecimalString(price),
                                     PrepaymentAssumption = prepayment.assumption)
   
    YieldToMaturity(mtg.cashflow) - yield
    }
    
    life.cpr <- try(
      uniroot(yieldcpr,
              interval = c(lower = 0, upper = .5),
              extendInt = 'yes',
              tol = .001,
              bond.id = bond.id,
              original.bal = original.bal,
              settlement.date = settlement.date,
              term.structure = term.structure,
              price = PriceDecimalString(price),
              yield = yield)$root
      )
    life.cpr = life.cpr * PSA.basis
    new("ModelToCPR",
        CPRLife = life.cpr)
  }
  