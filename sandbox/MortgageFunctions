#---------------------------
# Mortgage Payment Functions
#---------------------------

Mortgage.Monthly.Payment <- function(orig.bal = numeric(), note.rate = numeric(), term.mos = numeric()){
  
  #Error Trap Note Rate
  if (missing(note.rate))
    stop("Need to specify interest.rate as number between 0 and 1 for calculations.")
  if (!is.numeric(note.rate)  )
    stop("No numeric interest.rate specified.")
  
  note.rate = note.rate/12 
  term = term.mos
  pmt.factor = (1+note.rate)^term
  pmt = (orig.bal * pmt.factor) * (note.rate/(pmt.factor -1))
  pmt
}

Sched.Prin <- function(balance = numeric(), note.rate = numeric(), term.mos = numeric(), period = numeric()){
  note.rate = note.rate/12
  term = term.mos
  disc.pmt =  note.rate * (1+note.rate)^(period-1)
  disc.prin = ((1+note.rate)^(term))-1
  Scheduled.Prin = balance *(disc.pmt/disc.prin)
  Scheduled.Prin
}

Remain.Balance <- function(balance = numeric(), note.rate = numeric(), term.mos = numeric(), period = numeric()){
  note.rate = note.rate/12
  term = term.mos
  Remain.Balance = balance * ((((1+note.rate)^term) - ((1+note.rate)^period))/(((1+note.rate)^term.mos)-1))
  Remain.Balance
}

PPC.Ramp <- function(season.period = numeric(), begin.cpr = numeric(), end.cpr = numeric(), period = numeric()){
  if(end.cpr >= 1) {end.cpr = end.cpr/100 
                    begin.cpr = begin.cpr/100}
  monthly.cpr = (begin.cpr + ((period - 1) * (end.cpr-begin.cpr)/(season.period -1)))
  cpr = ifelse(monthly.cpr <= end.cpr, monthly.cpr, end.cpr)
  cpr
}

SMM.To.CPR <- function(SMM = numeric()){
  if (missing(SMM))
    stop("Need to specify a SMM Value")
  if (!is.numeric(SMM)  )
    stop("No numeric SMM specified.")
  if (SMM <0 | SMM > 1)
    stop("No SMM specified.")
  SMM = 1-((1-SMM)^(12))
  return(SMM)
}

CPR.To.SMM <- function(CPR = numeric()){
  if (missing(CPR))
    stop("Need to specify a SMM Value")
  if (!is.numeric(CPR)  )
    stop("No numeric SMM specified.")
  if (CPR <0 | CPR > 1)
    stop("No SMM specified.")
  CPR = 1-((1-CPR)^(1/12))
  return(CPR)
}

SMMVector.To.CPR <- function(SMM = vector(), num.period = numeric()){
  # This function yields the average SMM  
  SMM = prod(1 + SMM)^(1/num.period)
  SMM = SMM - 1
  SMMVector.to.CPR = 1-(1-SMM)^12 
}