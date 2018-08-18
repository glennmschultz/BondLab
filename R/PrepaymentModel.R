
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


  #' @include MBSDetails.R MortgageCashFlow.R
  NULL
  
  #' @title ProjectMortgage Rate
  #' @description Projects the forward mortgage rate used by the Bond Lab prepayment
  #' model based on the two and ten year forward rates
  #' @family Interest Rate Model
  #' @param bond.id an object of the type MBSDetails
  #' @param term.structure an object of the type TermStructure
  #' @export ProjectMortgageRate
  ProjectMortgageRate <- function(bond.id, term.structure){
    thirty.year.intercept = 2.25
    twenty.year.intercept = 2.00
    fifteen.year.intercept = 1.75
    ten.year.coeff = .75
    two.year.coeff = .06
    
    am.type = AmortizationType(bond.id)
    orig.term = as.character(AmortizationTerm(bond.id))
    sato = SATO(bond.id)
    wam = WAM(bond.id)
    
    MtgRate <- function(intercept, two.year.coeff, two.year.yield, ten.year.coefff, ten.year.yield, sato){
      mtgrate = intercept + sato + (two.year.coeff * two.year.yield) + (ten.year.coeff * ten.year.yield)
      return(mtgrate)
    }
    switch(am.type,
           fixed = switch(orig.term,
                          '30' = MtgRate(intercept = thirty.year.intercept, 
                                         two.year.coeff = two.year.coeff,
                                         two.year.yield = TwoYearForward(term.structure)[1:wam],
                                         ten.year.coeff = ten.year.coeff,
                                         ten.year.yield = TenYearForward(term.structure)[1:wam],
                                         sato = sato),
                          '20' = MtgRate(intercept = twenty.year.intercept, 
                                         two.year.coeff = two.year.coeff,
                                         two.year.yield = TwoYearForward(term.structure)[1:wam],
                                         ten.year.coeff = ten.year.coeff,
                                         ten.year.yield = TenYearForward(term.structure)[1:wam],
                                         sato = sato),
                          '15' = MtgRate(intercept = fifteen.year.intercept, 
                                  two.year.coeff = two.year.coeff,
                                  two.year.yield = TwoYearForward(term.structure)[1:wam],
                                  ten.year.coeff = ten.year.coeff,
                                  ten.year.yield = TenYearForward(term.structure)[1:wam],
                                  sato = sato)),
           arm = switch(orig.term,
                        '30' =0))
  }

  #' A S4 Class prepayment vectors which are passed to cash flow engines
  #' 
  #' The PrepaymentClass class is used to pass the prepayment information 
  #' and SMM vectors to the MortgageCashFlow engine.  It must be called in 
  #' advance for cash flow calculations.
  #' @slot PrepaymentAssumption A character string the type of 
  #' prepayment assumption used this may be "CPR", "PPC", or "MODEL"
  #' @slot PPCStart A numeric value the PPC starting CPR assumption.
  #' This is populated when PPC option is choosen
  #' @slot PPCEnd A numeric value the PPC ending CPR assumption.
  #' This is populated when PPC option is choosen
  #' @slot PPCSeasoning A numeric value the length of the PPC ramp.
  #' This is populated when PPC option is choosen
  #' @slot FirstPmtDate A character string the first payment date
  #' @slot LastPmtDate A character string the date of the last payment 
  #' received by the investor
  #' @slot FinalPmtDate A character string the date of the final payment 
  #' received by the investor
  #' @slot PmtDate A character string the payment date
  #' @slot LoanAge A numeric vector the projected loan age at each payment date
  #' @slot Period A numeric vector the index of the payment periods
  #' @slot NoteRate A numeric vector the note rate of the MBS pool or loan
  #' @slot MtgRateFwd A numeric vector the projected forward mortgage rate.
  #' @slot Incentive A numeric vector the projected incentive (refinance).  
  #' The difference between the note rate and the prevailing mortgage rate
  #' @slot SMM A numeric vector the projected SMM (Single Monthly Mortality).  
  #' SMM is the measure of voluntary repayments 
  #' @slot MDR A numeric vector the pojected MDR  (Monthly Default Rate)
  #' @slot Severity A numeric vector the loss severity given default
  #' @exportClass PrepaymentModel
  setClass("PrepaymentModel",
         representation(
           PrepaymentAssumption = "character",
           PPCStart = "numeric",
           PPCEnd = "numeric",
           PPCSeasoning = "numeric",
           FirstPmtDate = "character",
           LastPmtDate = "character",
           FinalPmtDate = "character",
           PmtDate = "character",
           LoanAge = "numeric",
           Period = "numeric",
           NoteRate = "numeric",
           MtgRateFwd = "numeric",
           Incentive = "numeric",
           SMM = "numeric",
           MDR = "numeric",
           Severity = "numeric"))

  #' A standard generic function to access the slot PrepaymentAssumption
  #' @param object An S4 class object of the type PrepaymentModel
  #' export PrepaymentAssumption 
  setGeneric("PrepaymentAssumption", function(object)
  {standardGeneric("PrepaymentAssumption")})
  
  #' A standard generic function to access the slot PPCstart
  #' @param object An S4 class object of the type PrepaymentModel
  #' export PPCStart
  setGeneric("PPCStart", function(object)
  {standardGeneric("PPCStart")})
  
  #' A standard generic function to access the slot PPCEnd
  #' @param object An S4 class object of the type PrepaymentModel
  #' export PPCEnd
  setGeneric("PPCEnd", function(object)
  {standardGeneric("PPCEnd")})
  
  #' A standard generic function to access the slot PPCSeasoning
  #' @param object An S4 class object of the type PrepaymentModel
  #' export PPCSeasoning
  setGeneric("PPCSeasoning", function(object)
  {standardGeneric("PPCSeasoning")})
  
  # Note: FirstPmtDate generic is defined in PassThroughConstructor.R
  # Note: LastPmtDate generic is defined in PassThroughConstructor.R
  # Note: FinalPmtdate generic is defined in the PassThroughConstructor.R
  # Note: PmtDate generic is defined in MortgageCashFlow.R 
  
  #' A standard generic function to access the slot LoanAge
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export LoanAge
  setGeneric("LoanAge", function(object)
    {standardGeneric("LoanAge")})
  
  # Note: Period generic is defined MortgageCashFlow.R
  
  #' A standard generic function to access the slot NoteRate
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export NoteRate
  setGeneric("NoteRate", function(object)
    {standardGeneric("NoteRate")})
  
  #' A standard generic function to access the slot MtgRateFwd
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export MtgRateFwd 
  setGeneric("MtgRateFwd", function(object)
    {standardGeneric("MtgRateFwd")})
  
  #' A standard generic function to access the slot Incentive
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export Incentive
  setGeneric("Incentive", function(object)
    {standardGeneric("Incentive")})
  
  #' A standard generic function to access the slot SMM
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export SMM
  setGeneric("SMM", function(object)
    {standardGeneric("SMM")})
  
  #' A standard generic function to replace the values of the slot SMM
  #' @param object An S4 class object of the type PrepaymentModel
  #' @param value the replacement value of the slot
  #' @export SMM<-
  setGeneric("SMM<-", function(object, value)
    {standardGeneric("SMM<-")})
  
  #' A standard generic function to access the slot MDR
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export MDR
  setGeneric("MDR", function(object)
    {standardGeneric("MDR")})
  
  #' A standard generic function to replace the value of the slot MDR
  #' @param object An S4 class object of the type PrepaymentModel
  #' @param value the replacement value of the slot
  #' @export MDR<-
  setGeneric("MDR<-", function(object, value)
    {standardGeneric("MDR<-")})
  
  #' A standard generic function to access the slot Severity
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export Severity
  setGeneric("Severity", function(object)
    {standardGeneric("Severity")})
  
  setMethod("initialize",
          signature("PrepaymentModel"),
          function(.Object,
                   PrepaymentAssumption = "character",
                   PPCStart = numeric(),
                   PPCEnd = numeric(),
                   PPCSeasoning = numeric(),
                   FirstPmtDate = "character",
                   LastPmtDate = "character",
                   FinalPmtDate = "character",
                   PmtDate = "character",
                   LoanAge = numeric(),
                   Period = numeric(),
                   NoteRate = numeric(),
                   MtgRateFwd = numeric(),
                   Incentive = numeric(),
                   SMM = numeric(),
                   MDR = numeric(),
                   Severity = numeric())
          { 
            callNextMethod(.Object,
                           PrepaymentAssumption = PrepaymentAssumption,
                           PPCStart = PPCStart,
                           PPCEnd = PPCEnd,
                           PPCSeasoning = PPCSeasoning,
                           FirstPmtDate = FirstPmtDate,
                           LastPmtDate = LastPmtDate,
                           FinalPmtDate = FinalPmtDate,
                           PmtDate = PmtDate,
                           LoanAge = LoanAge,
                           Period = Period,
                           NoteRate = NoteRate,
                           MtgRateFwd = MtgRateFwd,
                           Incentive = Incentive,
                           SMM = SMM,
                           MDR = MDR,
                           Severity = Severity)
          })
  
  #' A method to extact PrepaymentAssumption from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod PrepaymentAssumption
  setMethod("PrepaymentAssumption", signature("PrepaymentModel"),
            function(object){object@PrepaymentAssumption})
  
  #' A method to extract PPCStart from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod PPCStart
  setMethod("PPCStart", signature("PrepaymentModel"),
            function(object){object@PPCStart})
  
  #' A method to extract PPCEnd from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod PPCEnd
  setMethod("PPCEnd", signature("PrepaymentModel"),
            function(object){object@PPCEnd})
  
  #' A method to extract PPCSeasoning from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod PPCSeasoning
  setMethod("PPCSeasoning", signature("PrepaymentModel"),
            function(object){object@PPCSeasoning})
  
  #' A method to extract FirstPmtDate from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod FirstPmtDate
  setMethod("FirstPmtDate", signature("PrepaymentModel"),
            function(object){object@FirstPmtDate})
  
  #' A method to extract LastPmtDate from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod LastPmtDate
  setMethod("LastPmtDate", signature("PrepaymentModel"),
            function(object){object@LastPmtDate})
  
  #' A method to extract FinalPmtDate from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod FinalPmtDate
  setMethod("FinalPmtDate", signature("PrepaymentModel"),
            function(object){object@FinalPmtDate})
  
  #' A method to extract PmtDate from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod PmtDate
  setMethod("PmtDate", signature("PrepaymentModel"),
            function(object){object@PmtDate})
  
  #' A method to extract LoanAge from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod LoanAge
  setMethod("LoanAge", signature("PrepaymentModel"),
            function(object){object@LoanAge})
  
  #' A method to extract Period from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod Period
  setMethod("Period", signature("PrepaymentModel"),
            function(object){object@Period})
  
  #' A method to extract NoteRate from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod NoteRate
  setMethod("NoteRate", signature("PrepaymentModel"),
            function(object){object@NoteRate})
  
  #' A method to extract MtgRateFwd from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod MtgRateFwd
  setMethod("MtgRateFwd", signature("PrepaymentModel"),
            function(object){object@MtgRateFwd})
  
  #' A method to extract Incentive from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod Incentive
  setMethod("Incentive", signature("PrepaymentModel"),
            function(object){object@Incentive})
  
  #' A method to extract SMM from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod SMM
  setMethod("SMM", signature("PrepaymentModel"),
            function(object){object@SMM})
  
  #' A method to replace SMM value in the class PrepaymentModel
  #' @param object the name of the S4 class object
  #' @param value the replace value of the slot SMM
  #' @exportMethod SMM<-
  setReplaceMethod("SMM", signature("PrepaymentModel"),
                   function(object, value){
                     object@SMM <- value
                     return(object)
                   })
  
  #' A method to extract MDR from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod MDR
  setMethod("MDR", signature("PrepaymentModel"),
            function(object){object@MDR})
  
  #' A method to replace the MDR value in the class PrepaymentModel
  #' @param object The name of the S4 class object
  #' @param value the replacement value of the slot
  #' @exportMethod MDR<-
  setReplaceMethod("MDR", signature("PrepaymentModel"),
                   function(object, value){
                     object@MDR <- value
                     return(object)
                   })
  
  #' A method to extract Severity from class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod Severity
  setMethod("Severity", signature("PrepaymentModel"),
            function(object){object@Severity}) 
  
  #--------------------------------------------------------------------------
  # The Bond Lab machine learning prepayment model is called based on the the
  # sector type of MBS in the MBS Details object.
  #--------------------------------------------------------------------------
  #' A contstructor function for the PrepaymentModel object
  #' 
  #' The function is a constructor function for the PrepaymentModel object
  #' @param bond.id A character string referring to an object 
  #' of the type MBSDetails
  #' @param term.structure A character string referring to an object 
  #' of the type TermStructure
  #' @param ModelTune A character string the prepayment model object
  #' @param Burnout A numeric value the burnout variable
  #' @param PrepaymentAssumption A character string the prepayment assumption 
  #' used "model", "ppc", or "cpr"
  #' @param ... Optional values when "ppc" or "cpr" is used
  #' @param begin.cpr A numeric value the beginning CPR assumption
  #' @param end.cpr A numeric value the ending CPR assumption
  #' @param seasoning.period A numeric value the length of the seasoning ramp
  #' @param cpr A numeric value the CPR assumption (annual prepayment rate)
  #' @param cdr A numeric value the CDR assumption (annual default rate)
  #' @param severity A numeric value the loss severity given default
  #' @export PrepaymentModel
  PrepaymentModel <- function(bond.id, 
                              term.structure, 
                              ModelTune,
                              Burnout = 0, 
                              PrepaymentAssumption = 'MODEL' ,
                              ...,
                              begin.cpr = .2, 
                              end.cpr = 6, 
                              seasoning.period = 30,
                              cpr = 6,
                              cdr = 1,
                              severity = .25){
    #Check for a valid prepayment assumption
    if(!PrepaymentAssumption %in% c("MODEL", "CPR", "PPC")) stop
    ("Not a Valid Prepayment Assumption")

    NoteRate = GWac(bond.id)
    sato = SATO(bond.id)
    AmortizationTerm = AmortizationTerm(bond.id)
    AmortizationType = AmortizationType(bond.id)
    OriginalLoanBalance = OrigLoanBal(bond.id)
    OrigLTV = OrigLTV(bond.id)
    FirstPmtDate = as.Date(FirstPrinPaymentDate(bond.id), "%m-%d-%Y")
    LastPmtDate = as.Date(LastPmtDate(bond.id), "%m-%d-%Y")
    FinalPmtDate = as.Date(FinalPmtDate(bond.id), "%m-%d-%Y")
    NextPmtDate = as.Date(NextPmtDate(bond.id), "%m-%d-%Y")
    WALA = as.numeric(WALA(bond.id))
    
    mtg.rate <- ProjectMortgageRate(bond.id = bond.id,
                                    term.structure = term.structure)[1:length]
    
    mtg.term = as.integer(difftime(FinalPmtDate,
                                   FirstPmtDate, units = "days")/days.in.month) + 1
    
    remain.term = as.integer(difftime(FinalPmtDate,
                                      LastPmtDate, units = "days")/days.in.month) + 1
    
    period = seq(from = 1, to = remain.term, by = 1)
    
    pmt.date = as.Date(NextPmtDate)  %m+% months(seq(from = 0, to = remain.term-1,
                                                    by = 1)) 
    
    loan.age = as.integer(difftime(as.Date(NextPmtDate) %m+% 
                                   months(seq(from = 1, to = remain.term, by = 1)),
                                   as.Date(FirstPmtDate), units = "days")/days.in.month) - 1
    
    note.rate =  as.numeric(rep(NoteRate, length(loan.age)))
    
    if(PrepaymentAssumption == "MODEL")
    {smm = 0
    severity = rep(severity, remain.term)
    } 
    else
    {if(PrepaymentAssumption == "PPC") 
    {smm = round(as.numeric(1-(1-PPC.Ramp(begin.cpr = begin.cpr,
                                          end.cpr = end.cpr,
                                          season.period = seasoning.period,
                                          period = LoanAge))^(1/months.in.year)),8)
    severity = rep(severity, remain.term)
    } 
      else
      {smm = round(rep(1-(1-cpr)^(1/12), remain.term),8)}
      severity = rep(severity, remain.term)
    }
    
    
    # this condition sets default to zero when the prepayment model is not used 
    # it allows for standard PPC and CPR assumptions
    if(PrepaymentAssumption != "MODEL"){
      mdr <- round(rep(CDR.To.MDR(CDR = cdr), remain.term),8)} else {
        mdr <- round(0,8)}
    
    new("PrepaymentModel",
        PrepaymentAssumption = as.character(PrepaymentAssumption),
        PPCStart = if(PrepaymentAssumption == "PPC") {begin.cpr} else {0},
        PPCEnd = if(PrepaymentAssumption == "PPC") {end.cpr} else {0},
        PPCSeasoning = if(PrepaymentAssumption == "PPC") {seasoning.period
        } else {0},
        NoteRate = as.numeric(NoteRate),
        FirstPmtDate = as.character(FirstPmtDate),
        LastPmtDate = as.character(LastPmtDate),
        FinalPmtDate = as.character(FinalPmtDate),
        Period = Period,
        PmtDate = as.character(PmtDate),
        LoanAge = as.numeric(LoanAge),
        MtgRateFwd = as.numeric(mtg.rate),
        Incentive = as.numeric(Incentive),
        SMM = as.numeric(smm),
        MDR = as.numeric(mdr),
        Severity = as.numeric(severity)
    )

  }