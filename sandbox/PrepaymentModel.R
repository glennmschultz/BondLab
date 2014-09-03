# ==================== Bond Lab Prepayment Model =======================
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
           SMM = "numeric")
    )

  setClass("PrepaymentModelTune",
         representation(
           TurnoverRate = "numeric",
           Turnover.alpha = "numeric",
           Turnover.beta = "numeric",
           Turnover.theta = "numeric",
           Seasonality.alpha = "numeric",
           Seasonality.theta = "numeric",
           Incentive.Fast.theta.1 = "numeric",
           Incentive.Fast.theta.2 = "numeric",
           Incentive.Fast.beta = "numeric",
           Incentive.Fast.eta = "numeric",
           Incentive.Slow.theta.1 = "numeric",
           Incentive.Slow.theta.2 = "numeric",
           Incentive.Slow.beta = "numeric",
           Incentive.Slow.eta = "numeric",
           Burnout.beta.1 = "numeric",
           Burnout.beta.2 = "numeric"
         ))

    setClass("MortgageRate",
         representation(
           yr30 = "function",
           yr15 = "function"
         ))


#    setGeneric("PrepaymentAssumption",
#           function(bond.id = "character", TermStructure = "character", MortgageRate = "character", ModelTune = "character", Burnout = numeric(),
#                    PrepaymentAssumption = "character",...,begin.cpr = numeric(), end.cpr = numeric(), 
#                    seasoning.period = numeric(), CPR = numeric())
#           {standardGeneric("PrepaymentAssumption")})

            
#    setGeneric("Prepayment.Model",
#           function(ModelTune = "character", LoanAge = numeric(), Month = numeric(), incentive = numeric(), 
#                    Burnout.maxincen = numeric()) {standardGeneric("Prepayment.Model")})  

#    setGeneric("Seasoning",
#           function (alpha = numeric(), beta = numeric (), theta = numeric(), LoanAge = numeric())
#           {standardGeneric("Seasoning")})


#    setGeneric("Seasonality",
#           function(alpha = numeric(), Month = numeric(), theta = numeric())
#           {standardGeneric("Seasonality")})

#    setGeneric("Borrower.Incentive",
#           function(incentive = numeric(), theta1 = numeric(), theta2 = numeric(), beta = numeric(), location = numeric())
#           {standardGeneric("Borrower.Incentive")})

#    setGeneric("Burnout",
#           function(beta1 = numeric(), beta2 = numeric(), MaxIncen = numeric(), LoanAge = numeric())
#           {standardGeneric("Burnout")})


#    setGeneric("PrepaymentModelTune", function(TurnoverRate = "numeric", Turnover.alpha = "numeric",
#                                           Turnover.beta = "numeric", Turnover.theta = "numeric", Seasonality.alpha = "numeric",
#                                           Seasonality.theta = "numeric", Incentive.Fast.theta.1 = "numeric", Incentive.Fast.theta.2 = "numeric",
#                                           Incentive.Fast.beta = "numeric", Incentive.Fast.eta = "numeric", Incentive.Slow.theta.1 = "numeric",
#                                           Incentive.Slow.theta.2 = "numeric", Incentive.Slow.beta = "numeric", Incentive.Slow.eta = "numeric",
#                                           Burnout.beta.1 = "numeric", Burnout.beta.2 = "numeric") {standardGeneric("PrepaymentModelTune")})


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
                       SMM = "numeric")
              {
                  .Object@PrepaymentAssumption = PrepaymentAssumption
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
                  .Object@SMM  = SMM
                  
                  return(.Object)
                  callNextMethod(.Object,...)
              })

    setMethod("initialize",
          signature("MortgageRate"),
          function(.Object,
                   yr30 = "function",
                   yr15 = "function")
          {
            .Object@yr30 = yr30
            .Object@yr15 = yr15
            
            return(.Object)
            callNextMethod(.Object,...)
          })



  setMethod("initialize",
          signature("PrepaymentModelTune"),
          function(.Object,
                   TurnoverRate = "numeric",
                   Turnover.alpha = "numeric",
                   Turnover.beta = "numeric",
                   Turnover.theta = "numeric",
                   Seasonality.alpha = "numeric",
                   Seasonality.theta = "numeric",
                   Incentive.Fast.theta.1 = "numeric",
                   Incentive.Fast.theta.2 = "numeric",
                   Incentive.Fast.beta = "numeric",
                   Incentive.Fast.eta = "numeric",
                   Incentive.Slow.theta.1 = "numeric",
                   Incentive.Slow.theta.2 = "numeric",
                   Incentive.Slow.beta = "numeric",
                   Incentive.Slow.eta = "numeric",
                   Burnout.beta.1 = "numeric",
                   Burnout.beta.2 = "numeric")
{
            .Object@TurnoverRate = TurnoverRate
            .Object@Turnover.alpha = Turnover.alpha
            .Object@Turnover.beta = Turnover.beta
            .Object@Turnover.theta = Turnover.theta
            .Object@Seasonality.alpha = Seasonality.alpha
            .Object@Seasonality.theta = Seasonality.theta
            .Object@Incentive.Fast.theta.1 = Fast.theta.1
            .Object@Incentive.Fast.theta.2 = Fast.theta.2
            .Object@Incentive.Fast.beta = Fast.beta
            .Object@Incentive.Fast.eta = Fast.eta
            .Object@Incentive.Slow.theta.1 = Slow.theta.1
            .Object@Incentive.Slow.theta.2 = Slow.theta.2
            .Object@Incentive.Slow.beta = Slow.beta
            .Object@Incentive.Slow.eta = Slow.eta
            .Object@Burnout.beta.1 = Burnout.beta.1
            .Object@Burnout.beta.2 = Burnout.beta.2
            
            return(.Object)
            callNextMethod(.Object,...)
          })


# ===============  This function is the prepayment model and serves as a constructor for the prepayment model vector ===============

PrepaymentAssumption <- function(bond.id = "character", TermStructure = "character", MortgageRate = "character", ModelTune = "character", 
                                 Burnout = numeric(), PrepaymentAssumption = "character", ...,begin.cpr = numeric(), end.cpr = numeric(), 
                                 seasoning.period = numeric(), CPR = numeric()){
  
  #Mortgage Rate is the call the to MortgageRDS.rds in the Prepayment Model folder.  Prepayment Assumption does not open a connection
  #to the MortgageRate.rds it must be open by the function that is calling Prepayment Model
  
  
  #Check for a valid prepayment assumption
  if(!PrepaymentAssumption %in% c("MODEL", "CPR", "PPC")) stop("Not a Valid Prepayment Assumption")
  PrepayAssumption <- PrepaymentAssumption    
  
  #Error Trap the CPR assumption
  if(PrepaymentAssumption == "CPR") if(CPR >=1) {CPR = CPR/100} else {CPR = CPR}
  #PPC function has error trapping feature so there is no need to error trap for PPC
  
  NoteRate = bond.id@GWac
  sato = bond.id@SATO
  FirstPmtDate = as.Date(bond.id@FirstPrinPaymentDate, "%m-%d-%Y")
  LastPmtDate = as.Date(bond.id@LastPmtDate, "%m-%d-%Y")
  FinalPmtDate = as.Date(bond.id@FinalPmtDate, "%m-%d-%Y")
  NextPmtDate = as.Date(bond.id@NextPmtDate, "%m-%d-%Y")
  
  col.names <- c("Period", "PmtDate", "LoanAge", "TwoYearFwd", "TenYearFwd", "MtgRateFwd", "SMM")
  
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
                           "30" = MortgageRate@yr30(two = TermStructure@TwoYearFwd[1:length(LoanAge)], ten = TermStructure@TenYearFwd[1:length(LoanAge)], sato = sato),
                           "15" = MortgageRate@yr15(two = TermStructure@TwoYearFwd[1:length(LoanAge)], ten = TermStructure@TenYearFwd[1:length(LoanAge)], sato = sato)
            ), # end first nested switch statement
            arm = switch(term, 
                         "30" = 0 ) # end second nested switch statement
    ) # end of "n" the switch logic
    
  }
  
  Mtg.Rate <- Mtg.Rate(TermStructure = TermStructure, type = bond.id@AmortizationType, term = bond.id@AmortizationTerm)
  Mtg.Rate <- Mtg.Rate[1:length(LoanAge)] # This is why I need to make class classflow array
  
  Incentive =  as.numeric(NoteRate - Mtg.Rate)
  Burnout = bond.id@Burnout
  
  
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
      SMM = as.numeric(SMM)
    )
  
    }



# ============================== The Bond Lab base prepayment model ============

Prepayment.Model <- function(ModelTune = "character", LoanAge = vector(), 
                             Month = vector(), incentive = vector(), Burnout.maxincen = numeric()){
  
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
  Fast <- Borrower.Incentive(incentive = incentive, theta1 = Fast.theta1, theta2 = Fast.theta2, beta = Fast.beta, location = Fast.location)
  Slow <- Borrower.Incentive(incentive = incentive, theta1 = Slow.theta1, theta2 = Slow.theta2, beta = Slow.beta, location = Slow.location)
  Burnout <-Burnout(beta1 = Burnout.beta1, beta2 = Burnout.beta2, MaxIncen = Burnout.maxincen, LoanAge = LoanAge)
  
  Refinance <- (Fast * Burnout) + (Slow * (1-Burnout))
  
  SMM = Refinance + Turnover    
  SMM <-pmax(0, SMM)    
  }

  
  # ----------------------------------------------------------------------------------------
  # Prepayment Model Functions.  These functions are used to build the base prepayment model
  # ----------------------------------------------------------------------------------------

  # ===================== Seasoning Function =============================

  # Seasoning function is a 3-parameter asymtote exponential function where
  # The three parameter asymptote is equivalent to the PPC ramp
  # a is the asymptote of the function
  # b is the intercept of the function
  # c is the point where the max CPR is achieved
  
  Seasoning <- function(alpha = numeric(), beta = numeric(), theta = numeric(), LoanAge = numeric()){
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




  #================ Seasonality Function =====================
  # Seasonality is modeled as a sin wave
  # a is the amplitude of the wave an set the maximum seasonal factor
  # Month is the calendar month (1..., 12) numeric
  # b is a location parameter shifts the peak values > 1 shift left values < 1 shift right  
  
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



  # =============================== Borrower Incentive Function ==================
  # arctanget function with a location parameter  
  Borrower.Incentive <- function(incentive = numeric(), theta1 = numeric(), theta2 = numeric(), beta = numeric(), location = numeric()) { 
  theta1 + theta2 * atan(incentive + pi * (beta * ((location - atan(incentive))/pi))) 
  }



  # =============================== Borrower Burnout Function =====================
  # Burnout is an exponentially decreasing function
  # a is the coefficient on the burnout varaible and b is the measure of burnout
  Burnout <- function(beta1 = numeric(), beta2= numeric(), MaxIncen = numeric(), LoanAge = numeric()){
  exp(beta1 * LoanAge +  beta2 * MaxIncen)
  }  




  # ============================ Mortgage Model Tuning Parameters ================



      PrepaymentModelTune <- function(TurnoverRate = "numeric",
                                      Turnover.alpha = "numeric",
                                      Turnover.beta = "numeric",
                                      Turnover.theta = "numeric",
                                      Seasonality.alpha = "numeric",
                                      Seasonality.theta = "numeric",
                                      Incentive.Fast.theta.1 = "numeric",
                                      Incentive.Fast.theta.2 = "numeric",
                                      Incentive.Fast.beta = "numeric",
                                      Incentive.Fast.eta = "numeric",
                                      Incentive.Slow.theta.1 = "numeric",
                                      Incentive.Slow.theta.2 = "numeric",
                                      Incentive.Slow.beta = "numeric",
                                      Incentive.Slow.eta = "numeric",
                                      Burnout.beta.1 = "numeric",
                                      Burnout.beta.2 = "numeric")
                                  {
                                    new("PrepaymentModelTune",
                                        TurnoverRate = TurnoverRate,
                                        Turnover.alpha = Turnover.alpha,
                                        Turnover.beta = Turnover.beta,
                                        Turnover.theta = Turnover.theta,
                                        Seasonality.theta = Seasonality.theta,
                                        Incentive.Fast.theta.1 = Incentive.Fast.theta.1,
                                        Incentive.Fast.theta.2 = Incentive.Fast.theta.2,
                                        Incentive.Fast.beta = Incentive.Fast.beta,
                                        Incentive.Fast.eta = Incentive.Fast.eta,
                                        Incentive.Slow.theta.1 = Incentive.Slow.theta.1,
                                        Incentive.Slow.theta.2 = Incentive.Slow.theta.2,
                                        Incentive.Slow.beta = Incentive.Slow.beta,
                                        Incentive.Slow.eta = Incentive.Slow.eta,
                                        Burnout.beta.1 = Burnout.beta.1,
                                        Burnout.beta.2 = Burnout.beta.2
                                    )
                                  }



        MakePrepaymentTune <- function(ModelName = "character",
                                       TurnoverRate = "numeric",
                                       Turnover.alpha = "numeric",
                                       Turnover.beta = "numeric",
                                       Turnover.theta = "numeric",
                                       Seasonality.alpha = "numeric",
                                       Seasonality.theta = "numeric",
                                       Incentive.Fast.theta.1 = "numeric",
                                       Incentive.Fast.theta.2 = "numeric",
                                       Incentive.Fast.beta = "numeric",
                                       Incentive.Fast.eta = "numeric",
                                       Incentive.Slow.theta.1 = "numeric",
                                       Incentive.Slow.theta.2 = "numeric",
                                       Incentive.Slow.beta = "numeric",
                                       Incentive.Slow.eta = "numeric",
                                       Burnout.beta.1 = "numeric",
                                       Burnout.beta.2 = "numeric")

          {
          ModelTune <- PrepaymentModelTune(TurnoverRate = TurnoverRate,
                                          Turnover.alpha = Turnover.alpha,
                                          Turnover.beta = Turnover.beta,
                                          Turnover.theta = Turnover.theta,
                                          Seasonality.alpha = Seasonality.alpha,
                                          Seasonality.theta = Seasonality.theta,
                                          Incentive.Fast.theta.1 = Incentive.Fast.theta.1,
                                          Incentive.Fast.theta.2 = Incentive.Fast.theta.2,
                                          Incentive.Fast.beta = Incentive.Fast.beta,
                                          Incentive.Fast.eta = Incentive.Fast.eta,
                                          Incentive.Slow.theta.1 = Incentive.Slow.theta.1,
                                          Incentive.Slow.theta.2 = Incentive.Slow.theta.2,
                                          Incentive.Slow.beta = Incentive.Slow.beta,
                                          Incentive.Slow.eta = Incentive.Slow.eta,
                                          Burnout.beta.1 = Burnout.beta.1,
                                          Burnout.beta.2 = Burnout.beta.2)
          
          connTune <- gzfile(description = paste("~/BondLab/PrepaymentModel/",ModelName,".rds", sep = ""))
          saveRDS(ModelTune, connTune)
          close(connTune)
            }

  # ============================= Mortgage Rate Function Class ===================



  MortgageRate <- new("MortgageRate",
                    yr30 = function(two = numeric(), ten = numeric(), sato = numeric()) {
                      2.25 + (.06 * two) + (.75 * ten) + sato
                    },
                    
                    yr15 = function(two = numeric(), ten = numeric()){
                      1.75 + (.06 * two) + (.75 * ten) + sato
                    }
)
    
