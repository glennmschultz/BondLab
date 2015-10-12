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
  #' @param original.bal A numeric value the original balance
  #' @param projected.cashflow A character string referring to an object type of MortgageCashFlows
  #' @param horizon.months A numeric value the investment horizon in months
  #' @export ForwardPassThrough
  ForwardPassThrough <- function(bond.id = "character",
                                 original.bal = numeric(),
                                 projected.cashflow = "character",
                                 horizon.months = numeric()){
    TempBond <- bond.id
    CurrentPeriod <- bond.id@WALA + horizon.months
    NextPeriod <-  CurrentPeriod + 1
    SchedPrincipal <- projected.cashflow@ScheduledPrin[1:horizon.months]
    PrepaidPrincipal <- projected.cashflow@PrepaidPrin[1:horizon.months]
    DefaultedPrincipal <- projected.cashflow@DefaultedPrin[1:horizon.months]
    TotalPrincipal <- sum(SchedPrincipal) + sum(PrepaidPrincipal) + sum(DefaultedPrincipal)
    
    TempBond@LastPmtDate <- as.character(format(as.Date(bond.id@FirstPmtDate, format = "%m-%d-%Y") %m+% months(CurrentPeriod), "%m-%d-%Y"))
    TempBond@NextPmtDate <- as.character(format(as.Date(bond.id@FirstPmtDate, format = "%m-%d-%Y") %m+% months(NextPeriod), "%m-%d-%Y"))
    TempBond@MBSFactor <- ((original.bal * bond.id@MBSFactor) - TotalPrincipal)/original.bal
    TempBond@WAM <- bond.id@WAM + horizon.months
    TempBond@WALA <- bond.id@WALA - horizon.months
    
    connTemp <-  gzfile(description = paste(system.file(package = "BondLab"),
                                            "/Temp_BondData/","TempPassThrough.rds", sep = ""))
    saveRDS(TempBond, connTemp)
    close(connTemp)
  }