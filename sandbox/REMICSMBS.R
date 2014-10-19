# ==== The code below creates the deal data structures for BondLabSMBS ==  

MakeRAID(DealName = "BondLabSMBS", 
             Issuer = "Bondlab", 
             DealPriceDate = "12-01-2012", 
             DealSettlementDate = "01-01-2013",
             Underwriter = "Bondlab",
             NumberofTranches = 2,
             NumberPacSchedules = 0,
             NumberofGroups = 1,
             DealSize = 200000000,
             CollateralAmount = 200000000)
      
    MakeCollateral(DealName = "BondLabSMBS",
                   Group = 1,
                   Cusip = list("bondlabMBS4"),
                   OrigBal = list("200000000"))
     
  MakeTranche(DealName = "BondLabSMBS",
              TrancheNumber = "1",
              TrancheName = "A",
              TranchePrincipal = "Pass_Through",
              TrancheInterest = "Fix",
              TranchePrincipalDesc = "NTL",
              TrancheInterestDesc = "Fix",
              Cusip = "BondLabSMBSIO",
              TrancheOrigBal = 200000000,
              TrancheDatedDate  = "01-01-2013",
              TrancheFirstPmtDate = "01-15-2013",
              TrancheLastPmtDate = "12-15-2042",
              TrancheNextPmtDate = "01-15-2013",
              TrancheCoupon = 4.00,
              Delay = 0,
              PrinPmtFrequency = 12,
              InterestPmtFrequency = 12,
              FloaterIndex = "999",
              PacLowBand = 000,
              PacHighBand = 000,
              Group = 1)
    
    MakeTranche(DealName = "BondLabSMBS",
                TrancheNumber = "2",
                TrancheName = "B",
                TranchePrincipal = "Pass_Through",
                TrancheInterest = "Fix",
                TranchePrincipalDesc = "PO",
                TrancheInterestDesc = "Fix",
                Cusip = "BondLabSMBSPO",
                TrancheOrigBal = 200000000,
                TrancheDatedDate  = "01-01-2013",
                TrancheFirstPmtDate = "01-15-2013",
                TrancheLastPmtDate = "12-15-2042",
                TrancheNextPmtDate = "01-15-2013",
                TrancheCoupon = 0.00,
                Delay = 0,
                PrinPmtFrequency = 12,
                InterestPmtFrequency = 12,
                FloaterIndex = "999",
                PacLowBand = 000,
                PacHighBand = 000,
                Group = 1)

    Tranche <- readRDS("~/BondLab/Tranches/BondLabSMBSIO.rds")
   
    MakeRDME(DealName = "BondLabSMBS",
             TrancheNumber = 1,
             Cusip = "BondLabSMBSIO",
             PaymentDate = "01-01-2013",
             Coupon = 4.0,
             Factor = 1)
    
    MakeRDME(DealName = "BondLabSMBS",
             TrancheNumber = 2,
             Cusip = "BondLabSMBSPO",
             PaymentDate = "1-01-2013",
             Coupon = 0,
             Factor = 1)

  
    RemicStructure("BondLabSMBS")   
    
    REMIC <- readRDS("~/BondLab/REMICData/BondLabSMBS.rds")

