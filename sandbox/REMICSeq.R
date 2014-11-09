# ==== The code below creates the deal data structures for BondLabSMBS ==  

MakeRAID(DealName = "BondLabSEQ", 
             Issuer = "Bondlab", 
             DealPriceDate = "12-01-2012", 
             DealSettlementDate = "01-01-2013",
             Underwriter = "Bondlab",
             NumberofTranches = 3,
             NumberPacSchedules = 0,
             NumberofGroups = 1,
             DealSize = 200000000,
             CollateralAmount = 200000000)
      
    MakeCollateral(DealName = "BondLabSEQ",
                   Group = 1,
                   Cusip = list("bondlabMBS4"),
                   OrigBal = list("200000000"))
     
    MakeTranche(DealName = "BondLabSEQ",
                TrancheNumber = "1",
                TrancheName = "A",
                TranchePrincipal = "Pass_Through",
                TrancheInterest = "Fix",
                TranchePrincipalDesc = "SEQ",
                TrancheInterestDesc = "Fix",
                Cusip = "BondLabSEQ1",
                TrancheOrigBal = 50000000,
                TrancheDatedDate  = "01-01-2013",
                TrancheFirstPmtDate = "01-15-2013",
                TrancheLastPmtDate = "12-15-2042",
                TrancheNextPmtDate = "01-15-2013",
                TrancheCoupon = 0.63,
                Delay = 15,
                PrinPmtFrequency = 12,
                InterestPmtFrequency = 12,
                FloaterIndex = "999",
                PacLowBand = 000,
                PacHighBand = 000,
                Group = 1,
                Schedule = FALSE)
      
      MakeTranche(DealName = "BondLabSEQ",
                  TrancheNumber = "2",
                  TrancheName = "B",
                  TranchePrincipal = "Pass_Through",
                  TrancheInterest = "Fix",
                  TranchePrincipalDesc = "SEQ",
                  TrancheInterestDesc = "Fix",
                  Cusip = "BondLabSEQ2",
                  TrancheOrigBal = 75000000,
                  TrancheDatedDate  = "01-01-2013",
                  TrancheFirstPmtDate = "01-15-2013",
                  TrancheLastPmtDate = "12-15-2042",
                  TrancheNextPmtDate = "01-15-2013",
                  TrancheCoupon = 1.41,
                  Delay = 15,
                  PrinPmtFrequency = 12,
                  InterestPmtFrequency = 12,
                  FloaterIndex = "999",
                  PacLowBand = 000,
                  PacHighBand = 000,
                  Group = 1,
                  Schedule = FALSE)
  
    MakeTranche(DealName = "BondLabSEQ",
                TrancheNumber = "3",
               TrancheName = "C",
               TranchePrincipal = "Pass_Through",
               TrancheInterest = "Fix",
               TranchePrincipalDesc = "SEQ",
                TrancheInterestDesc = "Fix",
                Cusip = "BondLabSEQ3",
                TrancheOrigBal = 75000000,
                TrancheDatedDate  = "01-01-2013",
                TrancheFirstPmtDate = "01-15-2013",
                TrancheLastPmtDate = "12-15-2042",
                TrancheNextPmtDate = "01-15-2013",
                TrancheCoupon = 2.72,
                Delay = 15,
                PrinPmtFrequency = 12,
                InterestPmtFrequency = 12,
                FloaterIndex = "999",
                PacLowBand = 000,
                PacHighBand = 000,
                Group = 1,
                Schedule = FALSE)
   
      MakeRDME(DealName = "BondLabSEQ",
               TrancheNumber = 1,
               Cusip = "BondLabSEQ1",
               PaymentDate = "01-01-2013",
               Coupon = 0.63,
               Factor = 1)
      
      MakeRDME(DealName = "BondLabSEQ",
               TrancheNumber = 2,
               Cusip = "BondLabSEQ2",
               PaymentDate = "1-01-2013",
               Coupon = 1.41,
               Factor = 1)
  
      MakeRDME(DealName = "BondLabSEQ",
            TrancheNumber = 3,
             Cusip = "BondLabSEQ3",
             PaymentDate = "1-01-2013",
             Coupon = 2.72,
             Factor = 1)

  
    RemicStructure("BondLabSEQ")   
    
    REMIC <- readRDS("~/BondLab/REMICData/BondLabSEQ.rds")

