
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.



  #' A S4 Class to hold prepayment vectors which are passed to cash flow engines
  #' 
  #' The PrepaymentAssumption class is used to pass the prepayment information and
  #' SMM vectors to the MortgageCashFlow engine.  It must be called in advance for
  #' cash flow calculations.
  #' @slot PrepaymentAssumption A character string the type of prepayment assumption
  #' used this may be "CPR", "PPC", or "MODEL"
  #' @slot PPCStart A numeric value the PPC starting CPR assumption.  This is populated when
  #' PPC option is choosen
  #' @slot PPCEnd A numeric value the PPC ending CPR assumption.  This is populated when
  #' PPC option is choosen
  #' @slot PPCSeasoning A numeric value the length of the PPC ramp.  This is populated when 
  #' PPC option is choosen
  #' @slot FirstPmtDate A character string the first payment date
  #' @slot LastPmtDate A character string the date of the last payment received by the investor
  #' @slot FinalPmtDate A character string the date of the final payment received by the investor
  #' @slot PmtDate A character string the payment date
  #' @slot LoanAge A numeric vector the projected loan age at each payment date
  #' @slot Period A numeric vector the index of the payment periods
  #' @slot NoteRate A numeric vector the projected note rate of the MBS pool or loan
  #' @slot MtgRateFwd A numeric vector the projected forward mortgage rate.
  #' @slot Incentive A numeric vector the projected incentive faced by the borrower.  
  #' The difference between the note rate and the prevailing mortgage rate
  #' @slot SMM A numeric vector the projected SMM (Single Monthly Mortality).  
  #' SMM is the measure of voluntary repayments 
  #' @slot MDR A numeric vector the pojected MDR  (Monthly Default Rate)
  #' @slot Severitiy A numeric vector the loss severity given default 
  setClass("PrepaymentAssumption",
         representation(
           PrepayAssumption = "character",
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

  setMethod("initialize",
          signature("PrepaymentAssumption"),
          function(.Object,
                   PrepayAssumption = "character",
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
                   Severity = "numeric")
          { 
            callNextMethod(.Object,
                           PrepayAssumption = PrepayAssumption,
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


  #-------------------------------------------------------------------------------------
  #This section is the involuntary (default) repayment functions
  #-------------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------------
  # Default Baseline is a piece wise function.
  # The user selects the begining, peak CDR, the peak month, ramp plateau, and end End CDR
  #-------------------------------------------------------------------------------------
  CDR.Baseline <- function(BeginCDR = numeric(),
                           PeakCDR = numeric(),
                           EndCDR = numeric(),
                           PeakMonth = numeric(),
                           PlateauMonths = numeric(),
                           EndMonth = numeric(),
                           LoanAge = numeric()) {
  
  UpRamp = PeakCDR - BeginCDR  
  DownRamp = EndCDR - PeakCDR
  DownRampMonths = EndMonth - (PeakMonth + PlateauMonths)
  PlateauEnd = PeakMonth + PlateauMonths
  ifelse(LoanAge <= PeakMonth, 0 + ((LoanAge-1) * (UpRamp / (PeakMonth - 1))),
  ifelse(LoanAge > PeakMonth & LoanAge <= PlateauEnd ,PeakCDR, 
  ifelse(LoanAge > PlateauEnd & LoanAge <= EndMonth, PeakCDR + (LoanAge - PlateauEnd) * (DownRamp/DownRampMonths),EndCDR)))}

  #------------------------------------------------------------------------------------
  #Original Loan to Value Default Multipliers
  #------------------------------------------------------------------------------------
  OrigMultiplier <- function(OrigLTV = numeric(),
                             MinOLTV = numeric(),
                             MaxOLTV = numeric(),
                             MinOrigMultiplier = numeric(),
                             MaxOrigMultiplier = numeric()){
    ifelse(OrigLTV > MaxOLTV, MaxOrigMultiplier,
    ifelse(OrigLTV > MinOLTV & OrigLTV <= MaxOLTV, 1.0, MinOrigMultiplier))}

  #------------------------------------------------------------------------------------
  #Updated Loan to Value Default Multiplier Function
  #------------------------------------------------------------------------------------
  UpdatedLTVMultiplier <- function(beta = numeric(), 
                                  OrigLTV = numeric(), 
                                  ULTV = numeric()){
  chgLTV = (OrigLTV - ULTV)/100
  exp(-beta * chgLTV)}

  #-----------------------------------------------------------------------------------
  #SATO Default Multiplier Function
  #-----------------------------------------------------------------------------------

  SATOMultiplier <- function(beta = numeric(), 
                             SATO = numeric()) {
  exp(beta * SATO)}

  #-----------------------------------------------------------------------------------
  # The Bond Lab base voluntary prepayment model
  # Tuning parameters are called from the PrepaymentModelTune Class
  #-----------------------------------------------------------------------------------
  Prepayment.Model <- function(ModelTune = "character",
                               LoanAge = vector(), 
                               Month = vector(), 
                               incentive = vector(), 
                               Burnout.maxincen = numeric()){
  
  PPMFunctions <- ModelFunctions()
      

  Turnover.Rate <- 1-(1 - TurnoverRate(ModelTune))^(1/12)
  
  SeasoningRamp <- SeasoningRamp(PPMFunctions)(alpha = TurnoverAlpha(ModelTune),
                                                    beta = TurnoverBeta(ModelTune),
                                                    theta = TurnoverTheta(ModelTune),
                                                    LoanAge = LoanAge)
  SeasonalFactor <- SeasonalFactors(PPMFunctions)(alpha = SeasonalityAlpha(ModelTune),
                                                  theta = SeasonalityTheta(ModelTune),
                                                  Month = Month)
  
  Turnover <- Turnover.Rate * SeasoningRamp * SeasonalFactor
  
  # Calculate the Borrower Refinance Response
  Fast <- ArcTanIncentive(PPMFunctions)(incentive = incentive,
                                        theta1 = IncentiveFastThetaOne(ModelTune),
                                        theta2 = IncentiveFastThetaTwo(ModelTune),
                                        beta = IncentiveFastBeta(ModelTune),
                                        location = IncentiveFastEta(ModelTune))

  Slow <- ArcTanIncentive(PPMFunctions)(incentive = incentive, 
                             theta1 = IncentiveSlowThetaOne(ModelTune), 
                             theta2 = IncentiveSlowThetaTwo(ModelTune), 
                             beta = IncentiveSlowBeta(ModelTune), 
                             location = IncentiveSlowEta(ModelTune))
  
  Burnout <- BorrowerBurnout(PPMFunctions)(beta1 = BurnoutBetaOne(ModelTune), 
                    beta2 = BurnoutBetaTwo(ModelTune), 
                    MaxIncen = Burnout.maxincen, 
                    LoanAge = LoanAge)
  
  Refinance <- (Fast * Burnout) + (Slow * (1-Burnout))
  
  SMM = Refinance + Turnover    
  SMM <-pmax(0, SMM)    
  }


  #-------------------------------------------------------------------------------
  #Bond Lab base default model
  #The default model tuning parameters are called from the prepayment model tune class
  #-------------------------------------------------------------------------------
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
    
    # -------------------- Default Model Tune Parameter -------------------------
    BeginCDR      = BeginCDR(ModelTune)
    PeakCDR       = PeakCDR(ModelTune)
    EndCDR        = EndCDR(ModelTune)
    PeakMonth     = PeakMonth(ModelTune)
    PlateauMonths = PlateauMonths(ModelTune)
    EndMonth      = EndMonth(ModelTune)
    MinOrigLTV    = MinOrigLTV(ModelTune)
    MaxOrigLTV    = MaxOrigLTV(ModelTune)
    MinOrigMultiplier = MinOrigMultiplier(ModelTune)
    MaxOrigMultiplier = MaxOrigMultiplier(ModelTune)
    SATO.beta =         SATOBeta(ModelTune)
    UpdatedLTV.beta =   UpdatedLTVBeta(ModelTune)
  
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
    #convert to a monthly default rate (hazard) before applying default multipliers
    #--------------------------------------------------------------------------
    
    #----------------------------Calculate Updated LTV ------------------------
    EstimatedSalePrice <- OrigLoanBalance/(OrigLTV/ltv.basis)
    ScheduledBalance <- AmortizationBalance(OrigLoanBalance = OrigLoanBalance,
                                            NoteRate = NoteRate,
                                            TermMos = Term,
                                            LoanAge = LoanAge)
   
    if(is.null(HomePrice) == TRUE) {UpdatedLTV = (ScheduledBalance / EstimatedSalePrice) * price.basis
    } else {UpdatedLTV = (ScheduledBalance / (EstimatedSalePrice * HomePrice)) * price.basis}
    
    Monthly.Default <- 1-(1 - (Default/PSA.basis))^(1/months.in.year)
  
    OrigCoeff <- OrigMultiplier(OrigLTV = OrigLTV,
                                MinOLTV = MinOrigLTV,
                                MaxOLTV = MaxOrigLTV,
                                MinOrigMultiplier = MinOrigMultiplier,
                                MaxOrigMultiplier = MaxOrigMultiplier)
   
    SATOCoeff <- SATOMultiplier(beta = SATO.beta, 
                                SATO = SATO)

    
    UpdatedCoeff <- UpdatedLTVMultiplier(beta = UpdatedLTV.beta, 
                                         OrigLTV = OrigLTV, 
                                         ULTV = UpdatedLTV)
   

    Multiplier = log(OrigCoeff) + log(SATOCoeff) + log(UpdatedCoeff)


    MDR = pmin(1, Monthly.Default * exp(Multiplier))
    return(MDR)}
  
  # ----------------------------------------------------------------------------
  #This section begins the bond lab prepayment model
  #The constructor for the prepayment model vector starts below
  # ----------------------------------------------------------------------------
  #' A contstructor function for the PrepaymentAssumption object
  #' 
  #' The function is a constructor function for the PrepaymentAssumption object
  #' @param bond.id A character string referring to an object of the type MBSDetails
  #' @param TermStructure A character string referring to an object of the type TermStructure
  #' @param MortgageRate A character string the input value of mortgagerate.rds.  Prepayment Assumption does
  #' open Mtg.Rate connection directly rather takes the argument as an object.
  #' @param ModelTune A character string the prepayment model tune object
  #' @param Burnout A numeric value the burnout variable
  #' @param PrepaymentAssumption A character string the prepayment assumption used "MODEL", "PPC", or "CPR"
  #' @param ... Optional values when "PPC" or "CPR" is used
  #' @param begin.cpr A numeric value the beginning CPR assumption
  #' @param end.cpr A numeric value the ending CPR assumption
  #' @param seasoning.period A numeric value the length of the seasoning ramp
  #' @param CPR A numeric value the CPR assumption (annual prepayment rate)
  #' @param CDR A numeric value the CDR assumption (annual default rate)
  #' @param HomePrice NULL do not override value
  #' @param Severity A numeric value the loss severity given default
  #' @export
  PrepaymentAssumption <- function(bond.id = "character", 
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
  
  # Severity is optional value passed to the model the default is 35%.  Should build a severity
  # model class like mortgage rate and scenario for severity.
  # Mortgage Rate is the call the to MortgageRDS.rds in the Prepayment Model folder.  
  # Prepayment Assumption does not open a connection
  # to the MortgageRate.rds it must be open by the function that is calling Prepayment Model
    
  #Check for a valid prepayment assumption
  if(!PrepaymentAssumption %in% c("MODEL", "CPR", "PPC")) stop("Not a Valid Prepayment Assumption")
  PrepayAssumption <- PrepaymentAssumption    
  
  #Error Trap the CPR assumption
  if(PrepaymentAssumption == "CPR") if(CPR >=1) {CPR = CPR/100} else {CPR = CPR}
  #PPC function has error trapping feature so there is no need to error trap for PPC
  
  
  NoteRate = bond.id@GWac
  sato = bond.id@SATO
  AmortizationTerm = bond.id@AmortizationTerm
  AmortizationType = bond.id@AmortizationType
  OriginalLoanBalance = bond.id@OrigLoanBal
  OrigLTV = bond.id@OrigLTV
  FirstPmtDate = as.Date(bond.id@FirstPrinPaymentDate, "%m-%d-%Y")
  LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
  NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  WALA = as.numeric(bond.id@WALA)
  
  col.names <- c("Period", "PmtDate", "LoanAge", "TwoYearFwd", "TenYearFwd", "MtgRateFwd", "SMM")
  
  #Here Mtg.Term is the term of the pass-through and may differ from the actual amortozation term
  #reported in the MBS details because loans as typicall seasoned a few months before pooling
  
  Mtg.Term = as.integer(difftime(FinalPmtDate, FirstPmtDate, units = "days")/days.in.month) + 1
  Remain.Term = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/days.in.month) + 1
  Period = seq(from = 1, to = Remain.Term, by = 1)
  PmtDate = as.Date(NextPmtDate)  %m+% months(seq(from = 0, to = Remain.Term-1, by = 1)) 
  LoanAge = as.integer(difftime(as.Date(NextPmtDate)  %m+% months(seq(from = 1, to = Remain.Term, by = 1)), 
                                as.Date(FirstPmtDate), units = "days")/days.in.month) + (WALA + 1)

  
  NoteRate =  as.numeric(rep(NoteRate, length(LoanAge)))
  sato = as.numeric(rep(sato, length(LoanAge)))
  
  Mtg.Rate <- function(TermStructure = "character", 
                       type = "character", 
                       term = numeric()) {
    if(term >= 25) {term = as.character(30)
    } else {term = as.character(15)}
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
                         "30" = 0 ) # end second nested switch statement
    ) # end of "n" the switch logic
    
  }
  
  Mtg.Rate <- Mtg.Rate(TermStructure = TermStructure, 
                       type = AmortizationType, 
                       term = AmortizationTerm)
  
  Mtg.Rate <- Mtg.Rate[1:length(LoanAge)] 
  #Length of mortgage rate is set to loan age vector.
  #This is why I need to make class classflow array maybe l- or sapply works here
  
  Incentive =  as.numeric(NoteRate - Mtg.Rate)
  Burnout = pmax(bond.id@Burnout,(Incentive * 100)-(sato * 100))
  
  
  if(PrepaymentAssumption == "MODEL")
  {SMM = Prepayment.Model(ModelTune = ModelTune, 
                          LoanAge = LoanAge, 
                          Month = as.numeric(format(PmtDate, "%m")), 
                          incentive = Incentive, 
                          Burnout.maxincen = Burnout)
  Severity = rep(Severity, Remain.Term)
  } 
  else 
  {if(PrepaymentAssumption == "PPC") 
  {SMM = as.numeric(1-(1-PPC.Ramp(begin.cpr = begin.cpr, 
                                  end.cpr = end.cpr, 
                                  season.period = seasoning.period, 
                                  period = LoanAge))^(1/12))
  Severity = rep(Severity, Remain.Term)
  } 
  else
  {SMM = rep(1-(1-CPR)^(1/12), Remain.Term)}
  Severity = rep(Severity, Remain.Term)
  }
  
  
  # this condition sets default to zero when the prepayment model is not used 
  # it allows for standard PPC and CPR assumptions
  if(PrepaymentAssumption != "MODEL"){MDR <- rep(CDR.To.MDR(CDR = CDR), Remain.Term)} else
                        {MDR <- Default.Model(ModelTune = ModelTune,
                        OrigLoanBalance = OriginalLoanBalance,
                        NoteRate = NoteRate,
                        Term = AmortizationTerm * months.in.year,
                        OrigLTV = OrigLTV, 
                        SATO = sato,  
                        LoanAge = LoanAge,
                       ...,
                       HomePrice = HomePrice)}

  new("PrepaymentAssumption",
      PrepayAssumption = as.character(PrepayAssumption),
      PPCStart = if(PrepaymentAssumption == "PPC") {begin.cpr} else {0},
      PPCEnd = if(PrepaymentAssumption == "PPC") {end.cpr} else {0},
      PPCSeasoning = if(PrepaymentAssumption == "PPC") {seasoning.period} else {0},
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