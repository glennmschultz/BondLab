# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

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
            .Object@PrepayAssumption = PrepayAssumption
            .Object@PPCStart = PPCStart
            .Object@PPCEnd = PPCEnd
            .Object@PPCSeasoning = PPCSeasoning
            .Object@FirstPmtDate = FirstPmtDate
            .Object@LastPmtDate = LastPmtDate
            .Object@FinalPmtDate = FinalPmtDate
            .Object@PmtDate = PmtDate
            .Object@LoanAge = LoanAge
            .Object@Period = Period
            .Object@NoteRate = NoteRate
            .Object@MtgRateFwd = MtgRateFwd
            .Object@Incentive = Incentive
            .Object@SMM = SMM
            .Object@MDR = MDR
            .Object@Severity = Severity
            
            return(.Object)
            callNextMethod(.Object,....)
          })

  #---------------------------------------------------------------------------------------
  #Prepayment Model Functions.  These functions are used to build the base prepayment model
  #Funtion include those for voluntary repayment and defaults
  #---------------------------------------------------------------------------------------

  #---------------------------------------------------------------------------------------
  #This section is the voluntary repayment functions
  #---------------------------------------------------------------------------------------

  #---------------------------------------------------------------------------------------
  # Seasoning function is a 3-parameter asymtote exponential function where
  # The three parameter asymptote is equivalent to the PPC ramp
  # a is the asymptote of the function
  # b is the intercept of the function
  # c is the point where the max CPR is achieved
  #---------------------------------------------------------------------------------------
  Seasoning <- function(alpha = numeric(), 
                        beta = numeric(), 
                        theta = numeric(), 
                        LoanAge = numeric()){

  
  if (missing(alpha))
    stop("Need to specify alpha tuning parameter.")
  if (!is.numeric(alpha)  )
    stop("No numeric alpha specified.")
  
  if (missing(beta))
    stop("Need to specify beta tuning parameter.")
  if (!is.numeric(beta)  )
    stop("No numeric beta specified.")
  
  if (missing(theta))
    stop("Need to specify theta tuning parameter.")
  if (!is.numeric(theta)  )
    stop("No numeric theta specified.")
  
  if (missing(LoanAge))
    stop("Need to specify theta tuning parameter.")
  if (!is.numeric(LoanAge)  )
    stop("No numeric theta specified.")
  
  alpha - beta * exp(-theta * LoanAge)}

  #------------------------------------------------------------------------------------------
  # Seasonality is modeled as a sin wave
  # a is the amplitude of the wave an set the maximum seasonal factor
  # Month is the calendar month (1..., 12) numeric
  # b is a location parameter shifts the peak values > 1 shift left values < 1 shift right
  #------------------------------------------------------------------------------------------
  Seasonality <- function( alpha = numeric(), Month = numeric(), theta= numeric()){
  
  if (missing(alpha))
    stop("Need to specify alpha tuning parameter.")
  if (!is.numeric(alpha)  )
    stop("No numeric alpha specified.")
  
  if (missing(Month))
    stop("Need to specify Month variable.")
  if (!is.numeric(Month)  )
    stop("No numeric alpha specified.")
  
  if (missing(theta))
    stop("Need to specify Month variable.")
  if (!is.numeric(theta)  )
    stop("No numeric alpha specified.")
  
  (1  + alpha *sin((pi/2 * (Month + theta - 3)) / 3 - 1))}

  #--------------------------------------------------------------------------------------
  # arctanget function with a location parameter
  #--------------------------------------------------------------------------------------
  Borrower.Incentive <- function(incentive = numeric(), 
                                 theta1 = numeric(), 
                                 theta2 = numeric(), 
                                 beta = numeric(), 
                                 location = numeric()){ 
  theta1 + theta2 * atan(incentive + pi * (beta * ((location - atan(incentive))/pi)))}

  #-------------------------------------------------------------------------------------
  # Burnout is an exponentially decreasing function
  # a is the coefficient on the burnout varaible and b is the measure of burnout
  #-------------------------------------------------------------------------------------
  Burnout <- function(beta1 = numeric(), 
                      beta2= numeric(), 
                      MaxIncen = numeric(), 
                      LoanAge = numeric()){
  exp(beta1 * LoanAge +  beta2 * MaxIncen)}


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
  
  TurnoverRate        = ModelTune@TurnoverRate                       
  Seasoning.alpha     = ModelTune@Turnover.alpha
  Seasoning.beta      = ModelTune@Turnover.beta 
  Seasoning.theta     = ModelTune@Turnover.theta
  Seasonality.alpha   = ModelTune@Seasonality.alpha
  Seasonality.theta   = ModelTune@Seasonality.theta
  Fast.theta1         = ModelTune@Incentive.Fast.theta.1  
  Fast.theta2         = ModelTune@Incentive.Fast.theta.2 
  Fast.beta           = ModelTune@Incentive.Fast.beta 
  Fast.location       = ModelTune@Incentive.Fast.eta
  Slow.theta1         = ModelTune@Incentive.Slow.theta.1 
  Slow.theta2         = ModelTune@Incentive.Slow.theta.2 
  Slow.beta           = ModelTune@Incentive.Slow.beta 
  Slow.location       = ModelTune@Incentive.Slow.eta
  Burnout.beta1       = ModelTune@Burnout.beta.1 
  Burnout.beta2       = ModelTune@Burnout.beta.2
  
  Turnover.Rate <- 1-(1 - TurnoverRate)^(1/12)
  
  Turnover <- Turnover.Rate * 
  Seasoning(alpha = Seasoning.alpha, beta = Seasoning.beta, theta = Seasoning.theta, LoanAge = LoanAge) *
  Seasonality(alpha = Seasonality.alpha, Seasonality.theta, Month = Month)
  
  # Calculate the Borrower Refinance Response
  Fast <- Borrower.Incentive(incentive = incentive, 
                             theta1 = Fast.theta1, 
                             theta2 = Fast.theta2, 
                             beta = Fast.beta, 
                             location = Fast.location)
  Slow <- Borrower.Incentive(incentive = incentive, 
                             theta1 = Slow.theta1, 
                             theta2 = Slow.theta2, 
                             beta = Slow.beta, 
                             location = Slow.location)
  Burnout <-Burnout(beta1 = Burnout.beta1, 
                    beta2 = Burnout.beta2, 
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
    
    # -------------------- Default Model Tune Parameter -------------------------
    BeginCDR      = ModelTune@BeginCDR
    PeakCDR       = ModelTune@PeakCDR
    EndCDR        = ModelTune@EndCDR
    PeakMonth     = ModelTune@PeakMonth
    PlateauMonths = ModelTune@PlateauMonths
    EndMonth      = ModelTune@EndMonth
    MinOrigLTV    = ModelTune@MinOrigLTV
    MaxOrigLTV    = ModelTune@MaxOrigLTV
    MinOrigMultiplier = ModelTune@MinOrigMultiplier
    MaxOrigMultiplier = ModelTune@MaxOrigMultiplier
    SATO.beta =         ModelTune@SATO.beta
    UpdatedLTV.beta =   ModelTune@UpdatedLTV.beta
  
    # This function returns the amortization vector of a mortgage it is exact for a fixed rate mortage but only an
    # estimate of the vector for an adjustable rate mortage sufficent for updated LTV due to amortization.
    
    AmortizationBalance = function(OrigLoanBalance = numeric(), 
                                   NoteRate = numeric(), 
                                   TermMos = numeric(), 
                                   LoanAge = numeric()){
    NoteRate = NoteRate/(months.in.year * 100)
    Term = TermMos
    Remain.Balance = OrigLoanBalance * 
      (((1+NoteRate)^Term - ((1+NoteRate)^LoanAge))/(((1+NoteRate)^Term)-1))
    }
    
    
          Default <- CDR.Baseline(BeginCDR = BeginCDR,
                                  PeakCDR = PeakCDR,
                                  EndCDR = EndCDR,
                                  PeakMonth = PeakMonth,
                                  PlateauMonths = PlateauMonths,
                                  EndMonth = EndMonth,
                                  LoanAge = LoanAge)
   
    #--------------------------------------------------------------------------
    #convert to a monthly default rate (hazard) before applying default multipliers
    #--------------------------------------------------------------------------
    
    #----------------------------Calculate Updated LTV ------------------------
    EstimatedSalePrice <- OrigLoanBalance/(OrigLTV/100)
    ScheduledBalance <- AmortizationBalance(OrigLoanBalance = OrigLoanBalance,
                                            NoteRate = NoteRate,
                                            TermMos = Term,
                                            LoanAge = LoanAge)
   
    if(is.null(HomePrice) == TRUE) {UpdatedLTV = (ScheduledBalance / EstimatedSalePrice) * 100} else
    {UpdatedLTV = (ScheduledBalance / (EstimatedSalePrice * HomePrice)) * 100}
    
    Monthly.Default <- 1-(1 - (Default/100))^(1/12)
    

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
  #' @param bond.id A character string the bond id or cusip
  #' @param TermStructure A character string the method used to fit the term structure
  #' @param MortgageRate A character string the input value of mortgagerate.rds.  Prepayment Assumption does
  #' open Mtg.Rate connection directly rather takes the argument as an object.
  #' @param ModelTune A character string the prepayment model tune object
  #' @param Burnout A numeric value the burnout variable
  #' @param PrepaymentAssumption A character string the prepayment assumption used "MODEL", "PPC", or "CPR"
  #' @param ... Optional values when "PPC" or "CPR" is used
  #' @param begin.cpr A numeric value the beginning CPR assumption
  #' @param end.cpr A numeric value the ending CPR assumption
  #' @param seasoning.period A numeric value the length of the seasoning ramp
  #' @param CPR A numeric value the CPR assumption
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
                                   HomePrice = NULL,
                                   Severity = numeric()){
  
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
  
  #
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
  
  col.names <- c("Period", "PmtDate", "LoanAge", "TwoYearFwd", "TenYearFwd", "MtgRateFwd", "SMM")
  
  #Here Mtg.Term is the term of the pass-through and may differ from the actual amortozation term
  #reported in the MBS details because loans as typicall seasoned a few months before pooling
  
  Mtg.Term = as.integer(difftime(FinalPmtDate, FirstPmtDate, units = "days")/days.in.month) + 1
  Remain.Term = as.integer(difftime(FinalPmtDate, LastPmtDate, units = "days")/days.in.month) + 1
  Period = seq(from = 1, to = Remain.Term, by = 1)
  PmtDate = as.Date(NextPmtDate)  %m+% months(seq(from = 0, to = Remain.Term-1, by = 1)) 
  LoanAge = as.integer(difftime(as.Date(NextPmtDate)  %m+% months(seq(from = 1, to = Remain.Term, by = 1)), 
                                FirstPmtDate, units = "days")/days.in.month) + 1
  
  NoteRate =  as.numeric(rep(NoteRate, length(LoanAge)))
  sato = as.numeric(rep(sato, length(LoanAge)))
  
  # Here the switch function is used to determine the mortgage function to propogate the forward mortgage rate
  # switch is used because there will be more than 2 or 3 rates in the future and if else will get messy
  # the switch function is encapsulated with prepayment assumption for now
  
  Mtg.Rate <- function(TermStructure = "character", type = "character", term = numeric()) {
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
  {SMM = Prepayment.Model(ModelTune = ModelTune, LoanAge = LoanAge, Month = as.numeric(format(PmtDate, "%m")), 
                          incentive = Incentive, Burnout.maxincen = Burnout)} 
  else 
  {if(PrepaymentAssumption == "PPC") 
  {SMM = as.numeric(1-(1-PPC.Ramp(begin.cpr = begin.cpr, end.cpr = end.cpr, 
                                  season.period = seasoning.period, period = LoanAge))^(1/12))} 
  else
  {SMM = rep(1-(1-CPR)^(1/12), Remain.Term)}
  
  }
  
  
  # this condition set default to zero when the prepayment model is not used 
  # it allows for PSA and CPR assumptions
  if(PrepaymentAssumption != "MODEL"){MDR <- rep(0, Remain.Term)} else
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