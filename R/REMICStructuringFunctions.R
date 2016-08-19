
  #' A function to calcualte the cashflow of the REMIC collateral
  #' 
  #' Calculate the REMIC collateral cashflow
  #' @param bond.id a character the bond cusip or id
  #' @param settlement.date a character the settlement date
  #' @param term.structure a character refering to object of type TermStructure
  #' @param principal a character the current balance contributed to the REMIC
  #' @param PSA the PSA assumtion used to price the deal
  #' @export CollateralCashFlow
  CollateralCashFlow <- function(bond.id = "character", 
                                 settlement.date = "character",
                                 term.structure = "character",
                                 principal = numeric(),
                                 PSA = numeric()){
    
    MBS <- MBS(MBS.id = bond.id)
    TermStructure <- term.structure
    MortgageRate <- MtgRate()
    ModelTune <- ModelTune(MBS)
    
    Vector <- PrepaymentModel(
      bond.id = MBS,
      TermStructure = TermStructure,
      MortgageRate = MortgageRate,
      ModelTune = ModelTune,
      Burnout = BurnOut(MBS),
      PrepaymentAssumption = "PPC",
      begin.cpr = .002 * (PSA/PSA.basis),
      end.cpr = .06 * (PSA/PSA.basis),
      seasoning.period = 30)
    
    CollateralCashFlow <- CashFlowEngine(
      bond.id = MBS,
      settlement.date = settlement.date,
      principal = principal,
      PrepaymentAssumption = Vector)}

    
    #' A function to calculate the floater coupon 
    #' 
    #' The function calculates the floater coupon given
    #' @param cap a numeric value the floater cap
    #' @param floor a numeric value the floater floor
    #' @param margin a numeric value the floater margin
    #' @param index a numeric value the index value
    #' @param multiplier a numeric value the index multiplier
    #' @export FloatCoupon
    FloatCoupon <- function(cap, floor, margin, index, multiplier){
      min(cap, max((index * multiplier) + margin, floor))}