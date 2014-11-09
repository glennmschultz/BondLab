# ==== The code below creates the deal data structures for BondLabSMBS ==  

MakeRAID(DealName = "BondLabPAC01", 
             Issuer = "Bondlab", 
             DealPriceDate = "12-01-2012", 
             DealSettlementDate = "01-01-2013",
             Underwriter = "Bondlab",
             NumberofTranches = 2,
             NumberPacSchedules = 0,
             NumberofGroups = 1,
             DealSize = 200000000,
             CollateralAmount = 200000000)
      
    MakeCollateral(DealName = "BondLabPAC01",
                   Group = 1,
                   Cusip = list("bondlabMBS4"),
                   OrigBal = list("200000000"))
     
    MakeTranche(DealName = "BondLabPAC01",
                TrancheNumber = "1",
                TrancheName = "A",
                TranchePrincipal = "Pass_Through",
                TrancheInterest = "Fix",
                TranchePrincipalDesc = "PAC",
                TrancheInterestDesc = "Fix",
                Cusip = "BondLabPAC1",
                TrancheOrigBal = 148769215,
                TrancheDatedDate  = "01-01-2013",
                TrancheFirstPmtDate = "01-15-2013",
                TrancheLastPmtDate = "12-15-2042",
                TrancheNextPmtDate = "01-15-2013",
                TrancheCoupon = 3.0,
                Delay = 15,
                PrinPmtFrequency = 12,
                InterestPmtFrequency = 12,
                FloaterIndex = "999",
                PacLowBand = 000,
                PacHighBand = 000,
                Group = 1,
                Schedule = TRUE)
      
      MakeTranche(DealName = "BondLabPAC01",
                  TrancheNumber = "2",
                  TrancheName = "B",
                  TranchePrincipal = "Pass_Through",
                  TrancheInterest = "Fix",
                  TranchePrincipalDesc = "CMP",
                  TrancheInterestDesc = "Fix",
                  Cusip = "BondLabCMP1",
                  TrancheOrigBal = 51230785,
                  TrancheDatedDate  = "01-01-2013",
                  TrancheFirstPmtDate = "01-15-2013",
                  TrancheLastPmtDate = "12-15-2042",
                  TrancheNextPmtDate = "01-15-2013",
                  TrancheCoupon = 6.33,
                  Delay = 15,
                  PrinPmtFrequency = 12,
                  InterestPmtFrequency = 12,
                  FloaterIndex = "999",
                  PacLowBand = 000,
                  PacHighBand = 000,
                  Group = 1,
                  Schedule = TRUE)
  
      MakeRDME(DealName = "BondLabPAC01",
               TrancheNumber = 1,
               Cusip = "BondLabPAC1",
               PaymentDate = "01-01-2013",
               Coupon = 3.0,
               Factor = 1)
      
      MakeRDME(DealName = "BondLabPAC01",
               TrancheNumber = 2,
               Cusip = "BondLabCMP1",
               PaymentDate = "1-01-2013",
               Coupon = 6.33,
               Factor = 1)


  
    RemicStructure("BondLabPAC01")   
    
    REMIC <- readRDS("~/BondLab/REMICData/BondLabPAC01.rds")

    REMIC.Tranche <- readRDS("~/BondLab/Schedules/BondLabPAC01_Group_1_Sch.rds")

