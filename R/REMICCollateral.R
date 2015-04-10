# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

  
  REMICCollateral <- function(bond.id = "character", 
                            trade.date = "character",
                            settlement.date = "character",
                            collateral.price = numeric(),
                            PrepaymentAssumption = "character",
                            TermStructure = "character",
                            ..., 
                            begin.cpr = numeric(), 
                            end.cpr = numeric(), 
                            seasoning.period = numeric(), 
                            CPR = numeric(),
                            KeyRateTermStructure = NULL){
 
    # ---- connect to rates data folder
    rates.data <- Rates(trade.date = trade.date)
    
    # --- connect to mortgage rate class
    MortgageRate <- MtgRate()
   
    # --- call term structure 
                                       
    #Termstructure <- if(is.null(KeyRateTermStructure)) {TermStructure(rates.data = rates.data)} else {KeyRateTermStructure}
    Termstructure <- if(is.null(KeyRateTermStructure)) {TermStructure} else {KeyRateTermStructure}
    
    
    #-- call REMIC Trance
    REMIC.Tranche <- MBS(MBS.id = bond.id) 
  
    #-- call REMIC Deal Date
    REMIC.Deal <- REMICDeal(remic.deal = REMIC.Tranche@DealName)
  
  
    #    call collateral group details
    #    must be able to call multiple collateral groups or loan level detail 
    #    This is a function that calls multiple collateral groups
    #    and aggregates them into a single cash flow table for
    #    the REMIC collateral group cashflow
    #    how can I work with class MortgageCashFlow to aggregate results of more
    #    than one collateral group.  REMIC.Collateral.CashFlow function is the begining concept
    
    #----------------------------------------------------------------------------
    # REMIC.Collateral.CashFlow is an encapsulated function used to 
    REMIC.Collateral.CashFlow <- function(REMIC.Deal = "character",
                                        collateral.price = numeric(),
                                        settlement.date = "character",
                                        TermStructure = "character", 
                                        MortgageRate = "character",
                                        PrepaymentAssumption = "character", 
                                        ..., 
                                        begin.cpr = numeric(), 
                                        end.cpr = numeric(), 
                                        seasoning.period = numeric(), 
                                        CPR = numeric(),
                                        KeyRateTermStructure = "character"){    

  
  Group.Counter <- REMIC.Deal@NumberofGroups
  
  for(i in 1:Group.Counter){
  # --- connect to collateral  
  Collateral <- MBS(MBS.id = as.character(REMIC.Deal@Group[[i]]@Cusip))
  
  # --- connect to model tune
  ModelTune <- ModelTune(bond.id = Collateral)
  
  PrepaymentAssumption <- PrepaymentAssumption(bond.id = Collateral,
                                               TermStructure = TermStructure,
                                               MortgageRate = MortgageRate,
                                               ModelTune = ModelTune,
                                               Burnout = Collateral@Burnout,
                                               PrepaymentAssumption = PrepaymentAssumption,
                                               ...,
                                               begin.cpr = begin.cpr,
                                               end.cpr = end.cpr,
                                               seasoning.period = seasoning.period,
                                               CPR = CPR)
  
  #use this to plot OAS SMM vector - OAS vectors passed
  #plot((1-(1-PrepaymentAssumption@SMM)^12) * 100, type = "l") 
  
  Collateral.CashFlow <- MortgageCashFlow(bond.id = Collateral,
                                          original.bal = REMIC.Deal@CollateralAmount,
                                          settlement.date = settlement.date,
                                          price = collateral.price,
                                          PrepaymentAssumption = PrepaymentAssumption)
  
  }
  return(Collateral.CashFlow)
  }
  
   # call the above function and run collateral cash flow
   Collateral <- REMIC.Collateral.CashFlow(REMIC.Deal = REMIC.Deal,
                            collateral.price = collateral.price,
                            settlement.date = settlement.date,
                            TermStructure = Termstructure, 
                            MortgageRate = MortgageRate,
                            PrepaymentAssumption = PrepaymentAssumption,
                            ...,
                            begin.cpr = begin.cpr,
                            end.cpr = end.cpr,
                            seasoning.period = seasoning.period,
                            CPR = CPR,
                            KeyRateTermStructure = KeyRateTermStructure)
  }
  
  

  
  
        

