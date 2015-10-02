  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Glenn M Schultz, CFA

  # ==========================================================================================
  # These functions create temp horizon bond data for scenario analysis
  # the intention of these functions is to "roll forward" the cusip data for horizon
  # pricing - OAS, spot spread or nominal spread to the curve
  # ==========================================================================================
  
  #' A Function to "Roll Forward" the mortgage pass through cusip object (MBS Details)
  #' 
  #' A standard generic function to roll forward MBS cusip detail to the scenario horizon
  #' for pricing using spot spread, nominal spread, or option adjusted spread
  #' @param bond.id A character string referring to an object type of MBSDetails
  #' @param ProjectedCashFlow A character string referring to an object type of MortgageCashFlows
  ForwardPassThrough <- function(bond.id = "character",
                                 ProjectedCashFlow = "character",
                                 HorizonMonths = "numeric"){}