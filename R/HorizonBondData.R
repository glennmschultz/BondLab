
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage 
  # backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.

  # ===========================================================================
  # These functions create temp horizon bond data for scenario analysis
  # the intention of these functions is to "roll forward" the cusip data 
  # for horizon
  # pricing - OAS, spot spread or nominal spread to the curve
  # ===========================================================================
  
  #' A Function to "Roll Forward" the pass through cusip object (MBS Details)
  #' 
  #' A standard generic function to roll forward MBS cusip detail to the 
  #' scenario horizon
  #' for pricing using spot spread, nominal spread, or option adjusted spread
  #' @param bond.id A character string referring to an object type of MBSDetails
  #' @param original.bal A numeric value the original balance
  #' @param projected.cashflow A character string referring to an object type 
  #' of MortgageCashFlows
  #' @param horizon.months A numeric value the investment horizon in months
  #' @export ForwardPassThrough
  ForwardPassThrough <- function(bond.id = "character",
                                 original.bal = numeric(),
                                 projected.cashflow = "character",
                                 horizon.months = numeric()){
  TempBond <- bond.id
  # CurrentPeriod <- bond.id@WALA + horizon.months
  # NextPeriod <-  CurrentPeriod + 1
  SchedPrincipal <- ScheduledPrin(projected.cashflow)[1:horizon.months]
  PrepaidPrincipal <- PrepaidPrin(projected.cashflow)[1:horizon.months]
  DefaultedPrincipal <- DefaultedPrin(projected.cashflow)[1:horizon.months]
  TotalPrincipal <- sum(SchedPrincipal) + 
      sum(PrepaidPrincipal) + 
      sum(DefaultedPrincipal)
    
  LastPmtDate(TempBond) <- as.character(format(
    as.Date(LastPmtDate(bond.id), 
            format = "%m-%d-%Y") %m+% months(horizon.months), "%m-%d-%Y"))
    
  NextPmtDate(TempBond) <- as.character(format(
    as.Date(NextPmtDate(bond.id), 
            format = "%m-%d-%Y") %m+% months(horizon.months), "%m-%d-%Y"))
    
  MBSFactor(TempBond) <- ((original.bal * MBSFactor(bond.id)) - TotalPrincipal)/
    original.bal
  
  CurrentBal(TempBond) <- CurrentBal(bond.id) - TotalPrincipal
  
  WAM(TempBond) <- WAM(bond.id) - horizon.months
  WALA(TempBond) <- WALA(bond.id) + horizon.months
    
  connTemp <-  gzfile(description = paste(system.file(
    package = "BondLab"), "/Temp_BondData/","TempPassThrough.rds", sep = ""))
  saveRDS(TempBond, connTemp)
  close(connTemp)
  }