
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

  setGeneric("PrepaymentModel", function(bond.id = "character", 
                                         TermStructure = "character", 
                                         MortgageRate = "character",
                                         ModelTune = "character", 
                                         Burnout = numeric(), 
                                         PrepaymentAssumption = "character", 
                                         ...,
                                         begin.cpr = numeric(), 
                                         end.cpr = numeric(), 
                                         seasoning.period = numeric(), 
                                         CPR = numeric(),
                                         CDR = 0,
                                         HomePrice = NULL,
                                         Severity = 0)
  {standardGeneric("PrepaymentModel")})
  
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
  
  #' A standard generic function to access the slot MDR
  #' @param object An S4 class object of the type PrepaymentModel
  #' @export MDR
  setGeneric("MDR", function(object)
    {standardGeneric("MDR")})
  
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
  
  #' A method to extract MDR from S4 class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod MDR
  setMethod("MDR", signature("PrepaymentModel"),
            function(object){object@MDR})
  
  #' A method to extract Severity from class PrepaymentModel
  #' @param object the name of the S4 class
  #' @exportMethod Severity
  setMethod("Severity", signature("PrepaymentModel"),
            function(object){object@Severity}) 
  
  #--------------------------------------------------------------------------
  # The Bond Lab base voluntary prepayment model
  # Tuning parameters are called from the PrepaymentModelTune Class
  #--------------------------------------------------------------------------
  
  Voluntary.Model <- function(ModelTune = "character",
                               LoanAge = vector(), 
                               Month = vector(), 
                               incentive = vector(), 
                               Burnout.maxincen = numeric()){
  
  PPMFunctions <- ModelFunctions()
      

  Turnover.Rate <- 1-(1 - TurnoverRate(ModelTune))^(1/months.in.year)
  
  SeasoningRamp <- SeasoningRamp(PPMFunctions)(
    alpha = TurnoverAlpha(ModelTune),
    beta = TurnoverBeta(ModelTune),
    theta = TurnoverTheta(ModelTune),
    LoanAge = LoanAge)
  
  SeasonalFactor <- SeasonalFactors(PPMFunctions)(
    alpha = SeasonalityAlpha(ModelTune),
    theta = SeasonalityTheta(ModelTune),
    Month = Month)

  # Calculate the Borrower Refinance Response
  Fast <- ArcTanIncentive(PPMFunctions)(
    incentive = incentive,
    theta1 = IncentiveFastThetaOne(ModelTune),
    theta2 = IncentiveFastThetaTwo(ModelTune),
    beta = IncentiveFastBeta(ModelTune),
    location = IncentiveFastEta(ModelTune))

  Slow <- ArcTanIncentive(PPMFunctions)(
    incentive = incentive,
    theta1 = IncentiveSlowThetaOne(ModelTune),
    theta2 = IncentiveSlowThetaTwo(ModelTune),
    beta = IncentiveSlowBeta(ModelTune),
    location = IncentiveSlowEta(ModelTune))
  
  Burnout <- BorrowerBurnout(PPMFunctions)(
    beta1 = BurnoutBetaOne(ModelTune), 
    beta2 = BurnoutBetaTwo(ModelTune), 
    MaxIncen = Burnout.maxincen, 
    LoanAge = LoanAge)

  Refinance <- (log(Fast) * Burnout) + (log(Slow) * (1-Burnout))
  SeasoningRamp = log(SeasoningRamp)
  SeasonalFactor = log(SeasonalFactor)
  Curtailment = log(Curtailment(PPMFunctions)(LoanAge = LoanAge))
  
  SMM = Turnover.Rate * 
    exp(Refinance + SeasoningRamp + SeasonalFactor + Curtailment)
  }


  #----------------------------------------------------------------------------
  #Bond Lab base default model
  #The default model tuning parameters are called from the prepayment 
  #model tune class
  #----------------------------------------------------------------------------
  Default.Model <- function(ModelTune = "character",
                            OrigLoanBalance = numeric(),
                            NoteRate = numeric(),
                            Term = numeric(),
                            OrigLTV = numeric(),
                            SATO = numeric(),
                            LoanAge = vector(),
                            ...,
                            HomePrice = NULL){
    
    PPMFunctions <- ModelFunctions()
  
  # This function returns the amortization vector of a mortgage it is 
  # exact for a fixed rate mortage but only an estimate of the vector 
  # for an adjustable rate mortage sufficent for updated LTV due to amortization.
    
    AmortizationBalance = function(OrigLoanBalance = numeric(),
                                   NoteRate = numeric(),
                                   TermMos = numeric(),
                                   LoanAge = numeric()){
    NoteRate = NoteRate/(months.in.year * yield.basis)
    Term = TermMos
    Remain.Balance = OrigLoanBalance * 
      (((1+NoteRate)^Term - ((1+NoteRate)^LoanAge))/(((1+NoteRate)^Term)-1))
    }
  Default <-  DefaultRamp(PPMFunctions)(BeginCDR = BeginCDR(ModelTune),
                                  PeakCDR = PeakCDR(ModelTune),
                                  EndCDR = EndCDR(ModelTune),
                                  PeakMonth = PeakMonth(ModelTune),
                                  PlateauMonths = PlateauMonths(ModelTune),
                                  EndMonth = EndMonth(ModelTune),
                                  LoanAge = LoanAge)
   
  #--------------------------------------------------------------------------
  #convert to a monthly default rate before applying default multipliers
  #--------------------------------------------------------------------------
    
  #----------------------------Calculate Updated LTV ------------------------
  EstimatedSalePrice <- OrigLoanBalance/(OrigLTV/ltv.basis)
  ScheduledBalance <- AmortizationBalance(OrigLoanBalance = OrigLoanBalance,
                                            NoteRate = NoteRate,
                                            TermMos = Term,
                                            LoanAge = LoanAge)
   
  if(is.null(HomePrice) == TRUE) 
    {UpdatedLTV = (ScheduledBalance / EstimatedSalePrice) * price.basis
  } else {UpdatedLTV = (ScheduledBalance / (EstimatedSalePrice * HomePrice)) *
      price.basis}
    
  Monthly.Default <- 1-(1 - (Default/PSA.basis))^(1/months.in.year)
  
  OrigCoeff <- DefaultOrigLTVMult(PPMFunctions)(
    OrigLTV = OrigLTV,
    MinOLTV = MinOrigLTV(ModelTune),
    MaxOLTV = MaxOrigLTV(ModelTune),
    MinOrigMultiplier= MinOrigMultiplier(ModelTune),
    MaxOrigMultiplier = MaxOrigMultiplier(ModelTune))

    UpdatedCoeff <- 
      DefaultUpdatedLTVMult(PPMFunctions)(
        beta = UpdatedLTVBeta(ModelTune), 
         OrigLTV = OrigLTV,
         ULTV = UpdatedLTV)

    SATOCoeff <- DefaultSATOMult(PPMFunctions)(
      beta = SATOBeta(ModelTune),
      SATO = SATO)

    Multiplier = log(OrigCoeff) + log(SATOCoeff) + log(UpdatedCoeff)

    MDR = pmax(0, Monthly.Default * exp(Multiplier))
    return(MDR)}
  
  # --------------------------------------------------------------------------
  #This section begins the bond lab prepayment model
  #The constructor for the prepayment model vector starts below
  # --------------------------------------------------------------------------
  #' A contstructor function for the PrepaymentModel object
  #' 
  #' The function is a constructor function for the PrepaymentModel object
  #' @param bond.id A character string referring to an object 
  #' of the type MBSDetails
  #' @param TermStructure A character string referring to an object 
  #' of the type TermStructure
  #' @param MortgageRate A character string the input value of 
  #' mortgagerate.rds.  Prepayment Assumption does not
  #' open Mtg.Rate connection directly rather takes the argument as an object.
  #' @param ModelTune A character string the prepayment model tune object
  #' @param Burnout A numeric value the burnout variable
  #' @param PrepaymentAssumption A character string the prepayment assumption 
  #' used "MODEL", "PPC", or "CPR"
  #' @param ... Optional values when "PPC" or "CPR" is used
  #' @param begin.cpr A numeric value the beginning CPR assumption
  #' @param end.cpr A numeric value the ending CPR assumption
  #' @param seasoning.period A numeric value the length of the seasoning ramp
  #' @param CPR A numeric value the CPR assumption (annual prepayment rate)
  #' @param CDR A numeric value the CDR assumption (annual default rate)
  #' @param HomePrice NULL do not override value
  #' @param Severity A numeric value the loss severity given default
  #' @export PrepaymentModel
  PrepaymentModel <- function(bond.id = "character", 
                              TermStructure = "character", 
                              MortgageRate = "character",
                              ModelTune = "character", 
                              Burnout = numeric(), 
                              PrepaymentAssumption = "character", 
                              ...,
                              begin.cpr = numeric(), 
                              end.cpr = numeric(), 
                              seasoning.period = numeric(), 
                              CPR = numeric(),
                              CDR = 0,
                              HomePrice = NULL,
                              Severity = 0){
  
  # Severity is optional value passed to the model the default is 35%.  
  # Should build a severity model class like mortgage rate and scenario for 
  # severity. Mortgage Rate is the call the to MortgageRDS.rds in the 
  # Prepayment Model folder. Prepayment Assumption does not open a connection
  # to the MortgageRate.rds it must be open by the function that is calling 
  # Prepayment Model
    
  #Check for a valid prepayment assumption
  if(!PrepaymentAssumption %in% c("MODEL", "CPR", "PPC")) stop
    ("Not a Valid Prepayment Assumption")
  PrepaymentAssumption <- PrepaymentAssumption    
  
  #Error Trap the CPR assumption
  if(PrepaymentAssumption == "CPR") if(CPR >=1) {CPR = CPR/PSA.basis
  } else {CPR = CPR}
  # Need Error trap for PPC inputs
  
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
  
  col.names <- c("Period", "PmtDate", "LoanAge", 
                 "TwoYearFwd", "TenYearFwd", "MtgRateFwd", "SMM")
  
  # Here Mtg.Term is the term of the pass-through and may differ 
  # from the actual amortozation term
  # reported in the MBS details because loans as typicall seasoned a 
  # few months before pooling
  # note LoanAge is subtracted by 1 since the WALA is reported for the 
  # factor date WALA which is one month behind the current month
  
  Mtg.Term = as.integer(difftime(FinalPmtDate,
                            FirstPmtDate, units = "days")/days.in.month) + 1
  
  Remain.Term = as.integer(difftime(FinalPmtDate,
                              LastPmtDate, units = "days")/days.in.month) + 1
  
  Period = seq(from = 1, to = Remain.Term, by = 1)
  
  PmtDate = as.Date(NextPmtDate)  %m+% months(seq(from = 0, to = Remain.Term-1,
                                                  by = 1)) 
  
  LoanAge = as.integer(difftime(as.Date(NextPmtDate) %m+% 
              months(seq(from = 1, to = Remain.Term, by = 1)),
               as.Date(FirstPmtDate), units = "days")/days.in.month) - 1

  NoteRate =  as.numeric(rep(NoteRate, length(LoanAge)))
  sato = as.numeric(rep(sato, length(LoanAge)))
  
  Mtg.Rate <- function(TermStructure = "character",
                       type = "character",
                       term = numeric()){
    term = as.character(term)
    switch( type,
            fixed = switch(term,
  "30" = MortgageRate@yr30(two = TermStructure@TwoYearFwd[1:length(LoanAge)],
                           ten = TermStructure@TenYearFwd[1:length(LoanAge)],
                           sato = sato),
  "15" = MortgageRate@yr15(two = TermStructure@TwoYearFwd[1:length(LoanAge)],
                           ten = TermStructure@TenYearFwd[1:length(LoanAge)],
                           sato = sato)
            ), # end first nested switch statement
            arm = switch(term, 
                         "30" = 0) # end second nested switch statement
    ) # end of "n" the switch logic
    
  }

  Mtg.Rate <- Mtg.Rate(TermStructure = TermStructure, 
                       type = AmortizationType, 
                       term = AmortizationTerm)
  
  Mtg.Rate <- Mtg.Rate[1:length(LoanAge)] 
  #Length of mortgage rate is set to loan age vector.
  #This is why I need to make class classflow array 
  #maybe l- or sapply works here
  
  Incentive =  as.numeric(NoteRate - Mtg.Rate)
  Incentive <- ifelse(Incentive <= -4 , 
                      pmax(-4, Incentive), 
                      ifelse(Incentive >= 4, pmin(4, Incentive), Incentive))

  Burnout = pmax(bond.id@Burnout,(Incentive * yield.basis)-(sato * yield.basis))
  
  if(PrepaymentAssumption == "MODEL")
  {SMM = round(Voluntary.Model(ModelTune = ModelTune,
                         LoanAge = LoanAge,
                         Month = as.numeric(format(PmtDate, "%m")),
                         incentive = Incentive,
                         Burnout.maxincen = Burnout),8)
  Severity = rep(Severity, Remain.Term)
  } 
  else
  {if(PrepaymentAssumption == "PPC") 
  {SMM = round(as.numeric(1-(1-PPC.Ramp(begin.cpr = begin.cpr,
                                  end.cpr = end.cpr,
                                  season.period = seasoning.period,
                                  period = LoanAge))^(1/months.in.year)),8)
  Severity = rep(Severity, Remain.Term)
  } 
  else
  {SMM = round(rep(1-(1-CPR)^(1/12), Remain.Term),8)}
  Severity = rep(Severity, Remain.Term)
  }
  
  
  # this condition sets default to zero when the prepayment model is not used 
  # it allows for standard PPC and CPR assumptions
  if(PrepaymentAssumption != "MODEL"){
    MDR <- round(rep(CDR.To.MDR(CDR = CDR), Remain.Term),8)} else {
      MDR <- round(Default.Model(ModelTune = ModelTune,
                           OrigLoanBalance = OriginalLoanBalance,
                           NoteRate = NoteRate,
                           Term = AmortizationTerm * months.in.year,
                           OrigLTV = OrigLTV,
                           SATO = sato,
                           LoanAge = LoanAge,
                           ...,
                           HomePrice = HomePrice),8)}

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
      MtgRateFwd = as.numeric(Mtg.Rate),
      Incentive = as.numeric(Incentive),
      SMM = as.numeric(SMM),
      MDR = as.numeric(MDR),
      Severity = as.numeric(Severity)
  )
  
}