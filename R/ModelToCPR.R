
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
  #' @param TermStructure A character string referring to an object of type
  #' TermStructure.
  #' @param MortgageRate A character string referring to an object of type
  #' MortgageRate.
  #' @param ModelTune A character string referring to an object of type
  #' ModelTune.
  #' @param Burnout A numeric value the Burnout of the pool.
  #' @param original.bal A numeric value the original balance.
  #' @param settlement.date A character string the settlement date.
  #' @param price A numeric value the price.
  #' @param yield A numeric value the yield to maturity given a prepayment
  #' model vector
  #' @param ..., optional values
  #' @param iter A numeric value the number of iterations default = 100
  #' @param eps the tolerance default = 1e-08
  #' @export ModelToCPR
  ModelToCPR <- function(bond.id = "character",
                         TermStructure = "character",
                         MortgageRate = "character",
                         ModelTune = "character",
                         Burnout = numeric(),
                         original.bal = numeric(),
                         settlement.date = "character",
                         price = "character",
                         yield = "character",
                        ...,
                        iter = 100,
                        eps = 1e-08){

  cprvector = c(0, .35, .75)
  cpr2 = cprvector[2]
  i = 0

  for(i in 1:3){

    prepayment = PrepaymentModel(bond.id = bond.id,
                                 TermStructure = TermStructure,
                                 MortgageRate = MortgageRate,
                                 ModelTune = ModelTune,
                                 Burnout = Burnout,
                                 PrepaymentAssumption = "CPR",
                                 CPR = cprvector[i])


    yieldatcpr = YieldToMaturity(
      MortgageCashFlow(
        bond.id = bond.id,
        original.bal = original.bal,
        settlement.date = settlement.date,
        price = price,
        PrepaymentAssumption = prepayment))/yield.basis

    if(i==1) yield1 = yieldatcpr
      if(i==2) yield0 = yieldatcpr
        if(i ==3) yield2 = yieldatcpr
  }


  while ((abs(yield - yield0) > eps) & (i < iter)) {

    Prepayment = PrepaymentModel(
      bond.id = bond.id,
      TermStructure = TermStructure,
      MortgageRate = MortgageRate,
      ModelTune = ModelTune,
      Burnout = Burnout,
      PrepaymentAssumption = "CPR",
      CPR = cpr2)

    yield0 = YieldToMaturity(
      MortgageCashFlow(
        bond.id = bond.id,
        original.bal = original.bal,
        settlement.date = settlement.date,
        price = price,
        PrepaymentAssumption = Prepayment))/yield.basis

    yield2 = yield0

    cpr2 = abs((yield1 - yield)/(yield1 - yield2)) * abs(cprvector[1]- cpr2)
    i=i+1
  }

  if( i < iter) {LifeCPR = cpr2
  } else {LifeCPR = 999}

  new("ModelToCPR",
      CPRLife = LifeCPR)
}
