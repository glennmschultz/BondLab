# This function allocates the senior reduction amount for the FNMA CAS Deals
# the function allocates the senior reduction amounts along the deal data tree
# required inputs are subordination percentage, credit test trigger value, the 
# prorata allocation between the investor bond and issuer risk retention
# the maturity date of the bonds

CAS_Principal <- function(node, 
                          period = numeric(), 
                          CashFlowTable = "character",
                          CreditTestValue = numeric(),
                          ProRata = numeric(),
                          SeniorMaturity = date()){
  
  # --------------------------------------------------------------------
  # Get the scheduled and unscheduled principal for the current period
  # from the cash flow table.  The allocation of the senior and 
  # subordinated reduction amounts are found on page 77 and 78 of the prospectus
  # this deal pays the senior reduction amount on a sequential pro-rata pay basis
  # the subordinate reduction amount is also paid on a pro-rata basis.  The waterfall
  # allocates the senior allocation amount first to the node senior_prin.  The subordinate
  # allocation amount is paid to the node sub_prin.  The senior_prin and sub_prin amounts
  # are summed in each period node principal is the total principal paid to the investor
  # ---------------------------------------------------------------------
  Unscheduled = as.numeric(CashFlowTable[period,"Prepaid Prin"])
  Scheduled = as.numeric(CashFlowTable[period,"Scheduled Prin"])
  RecoverAmount = as.numeric(CashFlowTable[period, "Recovered Amount"])
  SubPercentage = SubordinationPercentage(period = period)
  SeniorPercentage =  1 - as.numeric(SubPercentage)
  
  # --------------------------------------------------------------------
  # Apply Credit Tests
  # --------------------------------------------------------------------
  CreditTest = CreditEnhancementTest(period = period, 
                                     CreditTestValue = CreditTestValue, 
                                     SubordinationPercentage = SubPercentage)
  
  # need to set-up the delinquency test for CAS delinquency vector
  
  # --------------------------------------------------------------------
  # Calculate Senior Reduction Amounts are applied sequentially to the senior bonds followed by 
  # the mezzanine bonds and the subordinated bond.  The calculation of the senior reduction amount
  # is outlined in the prospectus on page 78.
  # --------------------------------------------------------------------
  SeniorReduction = if(CreditTest != TRUE){
    (Scheduled * SeniorPercentage) + Unscheduled + RecoverAmount
  } else {(Scheduled * SeniorPercentage) + (Unscheduled * SeniorPercentage) + RecoverAmount}
  
  # --------------------------------------------------------------------
  # Calculate Subordinate Reduction Amounts
  # --------------------------------------------------------------------
  SubordinateReduction = (Unscheduled + Scheduled + RecoverAmount) - SeniorReduction
  
  # The notes mature on the final payment date
  #if(as.Date(Deal$collateral[period,"Date"], format = "%Y-%m-%d") == SeniorMaturity)
  
  # ---------------------------------------------------------------------------------------------------------------------------
  # First, allocate 1AH senior reduction amount the senoir reduction amount is paid sequentially pro rata
  # across the deal structure the payment rules are found on page 78 of the prospectus
  # ---------------------------------------------------------------------------------------------------------------------------
  if(as.numeric(Deal$`1AH`$BeginBal[period]) == 0){Deal$`1AH`$Senior_Prin[period] = 0 
  } else {Deal$`1AH`$Senior_Prin[period] <- min(as.numeric(Deal$`1AH`$BeginBal[period]), as.numeric(SeniorReduction))
  # Sequential pro rata rule for 1AH and 1M1/1M1H if collateral cash flow is greater than 1AH balance
  # then allocate the remaining principal to the 1M1 and 1M1H on a pro-rata basis
  Deal$`1M1`$Senior_Prin[period] <- max(0, (as.numeric(SeniorReduction) - as.numeric(Deal$`1AH`$BeginBal[period]))) * ProRata
  Deal$`1M1H`$Senior_Prin[period] <- max(0, (as.numeric(SeniorReduction) - as.numeric(Deal$`1AH`$BeginBal[period]))) * (1-ProRata)
  }
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Allocate 1M1/1M1H Senior Reduction Amount
  # --------------------------------------------------------------------------------------------------------------------------
  if(as.numeric(Deal$`1AH`$BeginBal[period]) == 0 & as.numeric(Deal$`1M1`$BeginBal[period]) != 0){
    Deal$`1M1`$Senior_Prin[period] <- min(as.numeric(Deal$`1M1`$BeginBal[period]), (as.numeric(SeniorReduction) * ProRata))
    Deal$`1M1H`$Senior_Prin[period] <- min(as.numeric(Deal$`1M1H`$BeginBal[period]), (as.numeric(SeniorReduction) * (1-ProRata))) 
    # Sequential pro rata rule for 1M2 and 1M2H 
    Deal$`1M2`$Senior_Prin[period] <- max(0, (as.numeric(SeniorReduction) * ProRata) - as.numeric(Deal$`1M1`$BeginBal[period]))
    Deal$`1M2H`$Senior_Prin[period] <- max(0, (as.numeric(SeniorReduction) * (1-ProRata)) - as.numeric(Deal$`1M1H`$BeginBal[period]))
  } else {
    Deal$`1M1`$Senior_Prin[period] <- 0
    Deal$`1M1H`$Senior_Prin[period] <- 0
  }
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Allocate 1M2/1M2H Senior Reduction Amount
  # --------------------------------------------------------------------------------------------------------------------------
  if(as.numeric(Deal$`1AH`$BeginBal[period]) == 0 & as.numeric(Deal$`1M1`$BeginBal[period]) == 0 & as.numeric(Deal$`1M2`$BeginBal[period]) != 0){
    Deal$`1M2`$Senior_Prin[period] <- min(as.numeric(Deal$`1M2`$BeginBal[period]), (as.numeric(SeniorReduction) * ProRata))
    Deal$`1M2H`$Senior_Prin[period] <- min(as.numeric(Deal$`1M2H`$BeginBal[period]), (as.numeric(SeniorReduction) * (1-ProRata))) 
    # Sequential pro rata rule for 1B1H
    Deal$`1B1H`$Senior_Prin[period] <- max(0, (as.numeric(SeniorReduction)) - 
                                             as.numeric(Deal$`1M2`$BeginBal[period]) - as.numeric(Deal$`1M2H`$BeginBal[period]))
  } else {
    Deal$`1M2`$Senior_Prin[period] <- 0
    Deal$`1M2H`$Senior_Prin[period] <- 0
  }
  
  # --------------------------------------------------------------------------------------------------------------------------
  # Allocate 1B1H Senior Reduction Amount
  # --------------------------------------------------------------------------------------------------------------------------
  if(as.numeric(Deal$`1AH`$BeginBal[period]) == 0 & as.numeric(Deal$`1M2`$BeginBal[period]) == 0){
    Deal$`1B1H`$Senior_Prin[period] <- min(as.numeric(Deal$`1B1H`$BeginBal[period]), (as.numeric(SeniorReduction) * ProRata))
  } else {
    Deal$`1B1H`$Senior_Prin[period] <- 0
  } 
  
  # --------------------------------------------------------------------------------------------------------------------------
  # The senior allocations rules end here and subordinate allocation rules begin below.
  # The subordinated allocation amount is paid sequentially pro rata across the deal strucuture
  # the subordinated payment rules are found on page 78 of the prospectus. 
  # --------------------------------------------------------------------------------------------------------------------------
  # ---------------------------------------------------------------------------------------------------------------------------
  # Allocate 1M1H Subordinate Reduction Amount
  # Allocation of the subordinate reduction amount needs to take into account any senior reduction amounts paid
  # in the period to the subordinated bond otherwise the payment rules will overpay the subordinated reduction amount  
  # ---------------------------------------------------------------------------------------------------------------------------
  if(as.numeric(Deal$`1M1`$BeginBal[period] == 0)){Deal$`1M1`$BeginBal[period] == 0
  }else{
    Deal$`1M1`$Sub_Prin[period] <- min(as.numeric(Deal$`1M1`$BeginBal[period]) - as.numeric(Deal$`1M1`$Senior_Prin), 
                                       (as.numeric(SubordinateReduction) * (ProRata)))
    Deal$`1M1H`$Sub_Prin[period] <- min(as.numeric(Deal$`1M1H`$BeginBal[period]) - as.numeric(Deal$`1M1H`$Senior_Prin), 
                                        (as.numeric(SubordinateReduction) * (1-ProRata)))
    # Sequential pro rata rule for 1M2 and 1M2H 
    Deal$`1M2`$Sub_Prin[period] <- max(0, ((as.numeric(SubordinateReduction) - as.numeric(Deal$`1M2`$Senior_Prin))  * (ProRata)) - 
                                         as.numeric(Deal$`1M1`$BeginBal[period]))
    Deal$`1M2H`$Sub_Prin[period] <- max(0, ((as.numeric(SubordinateReduction) - as.numeric(Deal$`1M2H`$Senior_Prin)) * (ProRata)) - 
                                          as.numeric(Deal$`1M1`$BeginBal[period]))} 
  
  # ----------------------------------------------------------------------------------------------------------------------------
  # Allocate 1M2 Subordinate Reduction Amount
  # The subordinated reduction amount needs to take into account any senior reduction amounts paid
  # in the period to the subordinated bond otherwise the payment rules will over pay the subordinated reduction amount
  # ----------------------------------------------------------------------------------------------------------------------------
  if(as.numeric(Deal$`1M1`$BeginBal[period]) == 0 & as.numeric(Deal$`1M2`$BeginBal[period] != 0)){
    Deal$`1M2`$Sub_Prin[period] <- min(as.numeric(Deal$`1M2`$BeginBal[period]) - as.numeric(Deal$`1M2`$Senior_Prin), 
                                       (as.numeric(SubordinateReduction) * ProRata))
    Deal$`1M2H`$Sub_Prin[period] <- min(as.numeric(Deal$`1M2H`$BeginBal[period]) - as.numeric(Deal$`1M2H`$Senior_Prin), 
                                        (as.numeric(SubordinateReduction) * (1-ProRata)))
    # Sequential pro rate rule for 1BH1
    Deal$`1B1H`$Sub_Prin[period] <- max(0, ((as.numeric(SubordinateReduction) - Deal$`1BH1`$Senior_Prin) * ProRata) - 
                                          Deal$`1M2`$BeginBal[period])} else {Deal$`1M2`$Principal[period] = 0}
  
  # -----------------------------------------------------------------------------------------------------------------------------
  # The final step is to the sum the senior principal amount and the subordinate principal amount
  # for each tranche.  The principal amount is passed to the EndingBal function.  This step is required for
  # CRT deals due to the fact that principal is actually bifurcated between senior and subordinated principal amounts
  # -----------------------------------------------------------------------------------------------------------------------------
  Deal$`1AH`$Principal[period] <- as.numeric(Deal$`1AH`$Senior_Prin[period]) + as.numeric(Deal$`1AH`$Sub_Prin[period])
  Deal$`1M1`$Principal[period] <- as.numeric(Deal$`1M1`$Senior_Prin[period]) + as.numeric(Deal$`1M1`$Sub_Prin[period])
  Deal$`1M1H`$Principal[period] <- as.numeric(Deal$`1M1H`$Senior_Prin[period]) + as.numeric(Deal$`1M1H`$Sub_Prin[period])
  Deal$`1M2`$Principal[period] <- as.numeric(Deal$`1M2`$Senior_Prin[period]) + as.numeric(Deal$`1M2`$Sub_Prin[period])
  Deal$`1M2H`$Principal[period] <- as.numeric(Deal$`1M2H`$Senior_Prin[period]) + as.numeric(Deal$`1M2H`$Sub_Prin[period])
  Deal$`1B1H`$Principal[period] <- as.numeric(Deal$`1B1H`$Senior_Prin[period]) + as.numeric(Deal$`1B1H`$Sub_Prin[period])
}
