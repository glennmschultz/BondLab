  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3
  # Copyright (C) 2014  Glenn M Schultz, CFA




  setGeneric("Mortgage.Monthly.Payment", function(orig.bal = numeric(),
                                                  note.rate = numeric(),
                                                  term.mos = numeric())
  {standardGeneric("Mortgage.Monthly.Payment")})
  
  #' A function to calculate the monthly payment of a mortgage
  #' 
  #' A standard generic function to compute the monthly payment of a mortgage
  #'@param orig.bal A numeric value, the original loan amount
  #'@param note.rate A numeric vlaue, the borrower's note rate
  #'@param term.mos A numeric value, the original term of the loan in months
  #'@examples Mortgage.Monthly.Payment(orig.bal = 100000, note.rate = .04, term.mos = 360)
  #' @export Mortgage.Monthly.Payment
  Mortgage.Monthly.Payment <- function(orig.bal = numeric(), 
                                     note.rate = numeric(), 
                                     term.mos = numeric()){
  #This function computes the monthly mortgage payment and can be used as part
  #Error Trap note rate.
  if (missing(note.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(note.rate)  )
    stop("No numeric interest.rate specified.")
  
  note.rate = note.rate/months.in.year
  term = term.mos
  pmt.factor = (1+note.rate)^term
  pmt = (orig.bal * pmt.factor) * (note.rate/(pmt.factor -1))
  return(pmt)

  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("Sched.Prin", function(balance = numeric(),
                                    note.rate = numeric(),
                                    term.mos = numeric(),
                                    period = numeric())
  {standardGeneric("Sched.Prin")})

  #' A function to calculate the scheduled principal of mortgage
  #' 
  #' A standard generic for the calculation of scheduled principal of mortgage loan
  #' @param balance A numeric value, the balance of the mortgage
  #' @param note.rate A numeric value, the borrower's note rate
  #' @param term.mos A numeric value, the term of the mortgage
  #' @param period A numeric value the number of months since the first payment
  #' @examples Sched.Prin(balance = 100000, note.rate = .045, term.mos = 360, period = 2)
  #' @export Sched.Prin
  Sched.Prin <- function(balance = numeric(), 
                       note.rate = numeric(), 
                       term.mos = numeric(), 
                       period = numeric()){
  
  #This function computes the monthly scheduled principal 
  #Requires additional error trapping
  note.rate = note.rate/months.in.year
  term = term.mos
  disc.pmt =  note.rate * (1+note.rate)^(period-1)
  disc.prin = ((1+note.rate)^(term))-1
  Scheduled.Prin = balance *(disc.pmt/disc.prin)
  return(Scheduled.Prin)
  }


  setGeneric("Remain.Balance", function(balance = numeric(), 
                                        note.rate = numeric(), 
                                        term.mos = numeric(), 
                                        period = numeric())
  {standardGeneric("Remain.Balance")})
  
  #' A function to compute the remaining balance of a mortgage
  #' 
  #' A standard generic for the calculation of a mortgage remaining balance
  #' @param balance A numeric value the balance of the mortgage
  #' @param note.rate A numeric value the borrower's note rate
  #' @param term.mos A numeric value the term of the mortgage
  #' @param period A numeric value the numeric of months from the first payment
  #' @examples Remain.Balance(balance = 100000, note.rate = .045, term.mos = 360, period = 10)
  #' @export Remain.Balance
  Remain.Balance <- function(balance = numeric(), 
                           note.rate = numeric(), 
                           term.mos = numeric(), 
                           period = numeric()){
  note.rate = note.rate/months.in.year
  term = term.mos
  Remain.Balance = balance * ((((1+note.rate)^term) - ((1+note.rate)^period))/(((1+note.rate)^term)-1))
  Remain.Balance
  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("PPC.Ramp", function(season.period = numeric(),
                                  begin.cpr = numeric(),
                                  end.cpr = numeric(),
                                  period = numeric())
  {standardGeneric("PPC.Ramp")})

  #' A function to calculate the PPC ramp
  #' 
  #' A standard generic function for the calculation of the PPC ramp
  #' @param season.period A numeric value the time to the peak PPC ramp
  #' @param begin.cpr A numeric value the starting CPR value
  #' @param end.cpr A numeric value the ending CPR value
  #' @param period A numeric value the current period (months from start date)
  #' @examples PPC.Ramp(season.period = 30, begin.cpr = .002, end.cpr = .06, period = 30) 
  #' @export PPC.Ramp
  PPC.Ramp <- function(season.period = numeric(), 
                     begin.cpr = numeric(), 
                     end.cpr = numeric(), 
                     period = numeric()){
  
  if(end.cpr >= 1) {end.cpr = end.cpr/100 
                    begin.cpr = begin.cpr/100}
  monthly.cpr = (begin.cpr + ((period - 1) * (end.cpr-begin.cpr)/(season.period -1)))
  cpr = ifelse(monthly.cpr <= end.cpr, monthly.cpr, end.cpr)
  return(cpr)
  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("SMM.To.CPR", function(SMM = numeric())
  {standardGeneric("SMM.To.CPR")})

  #' Convert the single monthly mortality rate SMM to CPR
  #' 
  #' A standard generic function used to convert the SMM to CPR
  #' @param SMM A numeric value the single monthly mortality rate (SMM)
  #' @examples SMM.To.CPR(SMM = .002)
  #' @export SMM.To.CPR
  SMM.To.CPR <- function(SMM = numeric()){
  if (missing(SMM))
    stop("Need to specify a SMM Value")
  if (!is.numeric(SMM)  )
    stop("No numeric SMM specified.")
  if (SMM <0 | SMM > 1)
    stop("No SMM specified.")
  SMM = 1-((1-SMM)^(months.in.year))
  return(SMM)
  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("CPR.To.SMM", function(CPR = numeric())
  {standardGeneric("CPR.To.SMM")})

  #' A function to convert the CPR to a single monthly mortality rate SMM
  #' 
  #' Standard Generic Function used to convert CPR to SMM 
  #' @param CPR A numeric value
  #' @examples
  #' CPR.To.SMM(CPR = .06)
  #' @export CPR.To.SMM
  CPR.To.SMM <- function(CPR = numeric()){
  if (missing(CPR))
    stop("Need to specify a SMM Value")
  if (!is.numeric(CPR)  )
    stop("No numeric SMM specified.")
  if (CPR <0 | CPR > 1)
    stop("No SMM specified.")
  CPR = 1-((1-CPR)^(1/months.in.year))
  return(CPR)
  }
  
  #-----------------------------------------------------------------------------------------------
  setGeneric("CDR.To.MDR", function(MDR = numeric())
  {standardGeneric("CDR.To.MDR")})
  
  #' A function to convert the CDR to a single monthly mortality rate MDR
  #' 
  #' Standard Generic Function used to convert CPR to SMM 
  #' @param CDR A numeric value
  #' @examples
  #' CDR.To.MDR(CDR = .06)
  #' @export CDR.To.MDR
  CDR.To.MDR <- function(CDR = numeric()){
    if (missing(CDR))
      stop("Need to specify a SMM Value")
    if (!is.numeric(CDR)  )
      stop("No numeric SMM specified.")
    if (CDR < 0 | CDR > 1)
      stop("No SMM specified.")
    MDR = 1-((1-CDR)^(1/months.in.year))
    return(MDR)
  }

  #-----------------------------------------------------------------------------------------------
  setGeneric("SMMVector.To.CPR", function(SMM = vector(), num.period = vector())
  {standardGeneric("SMMVector.To.CPR")})

  #' A function to convert  a time series of SMM to CPR
  #' 
  #' A standard generic function for the conversion a SMM time series to CPR
  #' to a time series of CPR.
  #' @param SMM A numeric vector of SMM
  #' @param num.period a numeric vector of period (Loan Age)
  #' @export SMMVector.To.CPR
  SMMVector.To.CPR <- function(SMM = vector(), 
                             num.period = vector()){
  # This function yields the average SMM  
  SMM = prod(1 + SMM)^(1/num.period)
  SMM = SMM - 1
  SMMVector.to.CPR = 1-(1-SMM)^months.in.year 
  }

 