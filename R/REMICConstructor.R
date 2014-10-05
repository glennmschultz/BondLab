# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

# Initialize RAID class
setMethod("initialize",
          signature("RAID"),
          function (.Object, 
                    DealName = "character",
                    Issuer = "character",
                    DealPriceDate = "character",
                    DealSettlementDate = "character",
                    Underwriter = "character",
                    NumberofTranches = numeric(),
                    NumberPacSchedules = numeric(),
                    NumberofGroups = numeric(),
                    DealSize = numeric(),
                    CollateralAmount = numeric())
          {
            
            .Object@DealName = DealName
            .Object@Issuer = Issuer
            .Object@DealPriceDate = DealPriceDate
            .Object@DealSettlementDate = DealSettlementDate
            .Object@Underwriter = Underwriter
            .Object@NumberofTranches = NumberofTranches
            .Object@NumberPacSchedules = NumberPacSchedules
            .Object@NumberofGroups = NumberofGroups
            .Object@DealSize = DealSize
            .Object@CollateralAmount = CollateralAmount
            
            return(.Object) 
            callNextMethod(.Object,...)               
          })

# Initialize TrancheDetails
setMethod("initialize",
          signature("TrancheDetails"),
          function(.Object,
                   DealName = "character",
                   TrancheNumber = "character",
                   TrancheName = "character",
                   TranchePrincipal = "character",
                   TrancheInterest = "character",
                   TranchePrincipalDesc = "character",
                   TrancheInterestDesc = "character",
                   Cusip = "character",
                   TrancheOrigBal = numeric(),
                   TrancheDatedDate = "character",
                   TrancheFirstPmtDate = "character",
                   TrancheLastPmtDate = "character",
                   TrancheNextPmtDate = "character",
                   TrancheCoupon = numeric(),
                   Delay = numeric(),
                   PrinPmtFrequency = numeric(),
                   InterestPmtFrequency = numeric(),
                   FloaterIndex = "character",
                   PacLowBand = numeric(),
                   PacHighBand = numeric(),
                   Group = numeric()){
            
            .Object@DealName = DealName
            .Object@TrancheNumber = TrancheNumber
            .Object@TrancheName = TrancheName
            .Object@TranchePrincipal = TranchePrincipal
            .Object@TrancheInterest = TrancheInterest
            .Object@TranchePrincipalDesc = TranchePrincipalDesc
            .Object@TrancheInterestDesc = TrancheInterestDesc
            .Object@Cusip = Cusip
            .Object@TrancheOrigBal = TrancheOrigBal
            .Object@TrancheDatedDate = TrancheDatedDate
            .Object@TrancheFirstPmtDate = TrancheFirstPmtDate
            .Object@TrancheLastPmtDate = TrancheLastPmtDate
            .Object@TrancheNextPmtDate = TrancheNextPmtDate
            .Object@TrancheCoupon = TrancheCoupon
            .Object@Delay = Delay
            .Object@PrinPmtFrequency = PrinPmtFrequency
            .Object@InterestPmtFrequency = InterestPmtFrequency
            .Object@FloaterIndex= FloaterIndex
            .Object@PacLowBand = PacLowBand
            .Object@PacHighBand = PacHighBand
            .Object@Group = Group
            
            return(.Object)
            callNextMethod(.Object,...)
          })

# Initialize Tranches
setMethod("initialize",
          signature("Tranches"),
          function(.Object,...,
                   Tranches = list())
          {
            .Object@Tranches = Tranches
            
            return(.Object)
            
            callNextMethod(.Object, ...)
          })

# Initialize collateral  
setMethod("initialize",
          signature ("Collateral"),
          function (.Object,
                    Group = numeric(),
                    Cusip = list(),
                    OrigBal = list()){
            
            .Object@Group = Group
            .Object@Cusip = Cusip
            .Object@OrigBal = OrigBal
            return(.Object) 
            
            callNextMethod(.Object,...)
          })

# Initialize collateralgroup
setMethod("initialize",
          signature("CollateralGroup"),
          function (.Object,
                    Group = list()) 
          {
            .Object@Group = Group
            return(.Object)
            
            callNextMethod(.Object,...)  
          })

#Initialize RDME
setMethod("initialize",
          signature("RDME"),
          function(.Object,
                   Cusip = "character",
                   PaymentDate = "character",
                   Coupon = numeric(),
                   Factor = numeric())
          {
            .Object@Cusip = Cusip
            .Object@PaymentDate = PaymentDate
            .Object@Coupon = Coupon
            .Object@Factor = Factor
            
            return(.Object)
            
            callNextMethod(.Object,...)  
            
          })

# Intitialize factors 
setMethod("initialize",
          signature("TrancheFactors"),
          function(.Object,
                   FactorData = list())
          {
            .Object@FactorData = FactorData
            return(.Object)
            
            callNextMethod(.Object,...)  
          })

# initialize REMIC structure superclass
setMethod("initialize",
          signature("REMICStructure"),
          function(.Object,
                   DealName = "character",
                   Issuer = "character",
                   DealPriceDate = "character",
                   DealSettlementDate = "character",
                   Underwriter = "character",
                   NumberofTranches = numeric(),
                   NumberPacSchedules = numeric(),
                   NumberofGroups = numeric(),
                   DealSize = numeric(),
                   CollateralAmount = numeric(),
                   Tranches = "character",
                   CollateralGroup = "character",
                   TrancheFactors ="character")
{
            .Object@DealName = DealName
            .Object@Issuer = Issuer
            .Object@DealPriceDate = DealPriceDate
            .Object@DealSettlementDate = DealSettlementDate
            .Object@Underwriter = Underwriter
            .Object@NumberofTranches = NumberofTranches
            .Object@NumberPacSchedules = NumberPacSchedules
            .Object@NumberofGroups = NumberofGroups
            .Object@DealSize = DealSize
            .Object@CollateralAmount = CollateralAmount
            .Object@Tranches = Tranches
            .Object@Group = CollateralGroup
            .Object@FactorData = TrancheFactors
            
            return(.Object)
            
            callNextMethod(.Object,...)           
          })


# Construct RAID class with call to new
  RAID <- function(DealName = "character",
                 Issuer = "character",
                 DealPriceDate = "character",
                 DealSettlementDate = "character",
                 Underwriter = "character",
                 NumberofTranches = numeric(),
                 NumberPacSchedules = numeric(),
                 NumberofGroups = numeric(),
                 DealSize = numeric(),
                 CollateralAmount = numeric()
                 ){
  
                 new("RAID",
                     DealName = DealName,
                     Issuer = Issuer,
                     DealPriceDate = DealPriceDate,
                     DealSettlementDate = DealSettlementDate,
                     Underwriter = Underwriter,
                     NumberofTranches = NumberofTranches,
                     NumberPacSchedules = NumberPacSchedules,
                     NumberofGroups = NumberofGroups,
                     DealSize = DealSize,
                     CollateralAmount = CollateralAmount
                     )                 
          }

# serialize the RAID class to the RAID directory
  MakeRAID <- function(DealName = "character", 
                     Issuer = "character", 
                     DealPriceDate = "character", 
                     DealSettlementDate = "character",
                     Underwriter = "character",
                     NumberofTranches = numeric(),
                     NumberPacSchedules = numeric(),
                     NumberofGroups = numeric(),
                     DealSize = numeric(),
                     CollateralAmount = numeric()){
  
  temp <-RAID(DealName = DealName,
              Issuer = Issuer,
              DealPriceDate = DealPriceDate,
              DealSettlementDate = DealSettlementDate,
              Underwriter = Underwriter,
              NumberofTranches = NumberofTranches,
              NumberPacSchedules = NumberPacSchedules,
              NumberofGroups = NumberofGroups,
              DealSize = DealSize,
              CollateralAmount = CollateralAmount)
  connRAID <- gzfile(description = paste("~/BondLab/RAID/",temp@DealName,".rds", sep = ""))
  saveRDS(temp, connRAID)
  close(connRAID)
  }
  

# 1) Construct tranche details with call to new
  TrancheDetails <- function( DealName = "character",
                              TrancheNumber = "character",
                              TrancheName = "character",
                              TranchePrincipal = "character",
                              TrancheInterest = "character",
                              TranchePrincipalDesc = "character",
                              TrancheInterestDesc = "character",
                              Cusip = "character",
                              TrancheOrigBal = numeric(),
                              TrancheDatedDate = "character",
                              TrancheFirstPmtDate = "character",
                              TrancheLastPmtDate = "character",
                              TrancheNextPmtDate = "character",
                              TrancheCoupon = numeric(),
                              Delay = numeric(),
                              PrinPmtFrequency = numeric(),
                              InterestPmtFrequency = numeric(),
                              FloaterIndex = "character",
                              PacLowBand = numeric(),
                              PacHighBand = numeric(),
                              Group = numeric()){
    
    new("TrancheDetails",
        DealName = DealName,
        TrancheNumber = TrancheNumber,
        TrancheName = TrancheName,
        TranchePrincipal = TranchePrincipal,
        TrancheInterest = TrancheInterest,
        TranchePrincipalDesc = TranchePrincipalDesc,
        TrancheInterestDesc = TrancheInterestDesc,
        Cusip = Cusip,
        TrancheOrigBal = TrancheOrigBal,
        TrancheDatedDate = TrancheDatedDate,
        TrancheFirstPmtDate = TrancheFirstPmtDate,
        TrancheLastPmtDate = TrancheLastPmtDate,
        TrancheNextPmtDate = TrancheNextPmtDate,
        TrancheCoupon = TrancheCoupon,
        Delay = Delay,
        PrinPmtFrequency = PrinPmtFrequency,
        InterestPmtFrequency = InterestPmtFrequency,
        FloaterIndex = FloaterIndex,
        PacLowBand = PacLowBand,
        PacHighBand = PacHighBand,
        Group = Group)
  }
  
  
# 2) serialize the tranches to the tranches directory  
  MakeTranche <- function(  DealName = "character",
                            TrancheNumber = "character",
                            TrancheName = "character",
                            TranchePrincipal = "character",
                            TrancheInterest = "character",
                            TranchePrincipalDesc = "character",
                            TrancheInterestDesc = "character",
                            Cusip = "character",
                            TrancheOrigBal = numeric(),
                            TrancheDatedDate = "character",
                            TrancheFirstPmtDate = "character",
                            TrancheLastPmtDate = "character",
                            TrancheNextPmtDate = "character",
                            TrancheCoupon = numeric(),
                            Delay = numeric(),
                            PrinPmtFrequency = numeric(),
                            InterestPmtFrequency = numeric(),
                            FloaterIndex = "character",
                            PacLowBand = numeric(),
                            PacHighBand = numeric(),
                            Group = numeric()) {
    
    temp <- TrancheDetails( DealName = DealName,
                            TrancheNumber = TrancheNumber,
                            TrancheName = TrancheName,
                            TranchePrincipal = TranchePrincipal,
                            TrancheInterest = TrancheInterest,
                            TranchePrincipalDesc = TranchePrincipalDesc,
                            TrancheInterestDesc = TrancheInterestDesc,
                            Cusip = Cusip,
                            TrancheOrigBal = TrancheOrigBal,
                            TrancheDatedDate = TrancheDatedDate,
                            TrancheFirstPmtDate = TrancheFirstPmtDate,
                            TrancheLastPmtDate = TrancheLastPmtDate,
                            TrancheNextPmtDate = TrancheNextPmtDate,
                            TrancheCoupon = TrancheCoupon,
                            Delay = Delay,
                            PrinPmtFrequency = PrinPmtFrequency,
                            InterestPmtFrequency = InterestPmtFrequency,
                            FloaterIndex = FloaterIndex,
                            PacLowBand = PacLowBand,
                            PacHighBand = PacHighBand,
                            Group = Group)
    
    connTranche <- gzfile(description = paste("~/BondLab/Tranches/",DealName,"_","Tranche","_",temp@TrancheNumber,".rds", sep = ""))
    saveRDS(temp, connTranche)
    close(connTranche)
  }
  

  
  # 3) tranches assembles the tranches for REMIC structure and is called by REMIC constructor function
  # The function assembles multiple tranches associated with a deal 
  # building the tranche classes into a list
  
  Tranches <- function(NumberofTranches = numeric(), DealName = "character"){
    
    TrancheList <- list()
    
    for(i in 1: NumberofTranches){
      
      connTranches <- gzfile(description = paste("~/BondLab/Tranches/",DealName,"_","Tranche", "_",i,".rds", sep = "")) 
      Tranches <- readRDS(connTranches)
      
      TrancheList <- append(TrancheList, Tranches)}
    
    new("Tranches",
        Tranches = TrancheList)
    
    #close(connTranches)
    #closing conn causes this function to return null, why?
  }
  

  
  # 1) construct the collateral class with call to new
    
  Collateral <- function(DealName = "character", 
                         Group = numeric(), 
                         Cusip = list(), 
                         OrigBal = list()){
    new("Collateral",
        Group = as.numeric(Group),
        Cusip = as.list(Cusip),
        OrigBal = as.list(OrigBal)
    )}
  

  
  # 2) serialize the collateral information to the groups directory
  MakeCollateral <- function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list()){
    
    temp <- Collateral(DealName = DealName, Group = Group, Cusip = Cusip, OrigBal = OrigBal)
    
    connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group","_",temp@Group,".rds", sep = ""))
    saveRDS(temp, connGroup)
    
  }

  # 3) aggregator function for the REMIC structure called by REMIC constructor
  # the function assembles multiple collateral groups can be extended to loan level 
  CollateralGroup <- function(NumberofGroups = numeric(), DealName = "character"){
    
    GroupList <- list()
    
    for(i in 1 : NumberofGroups){
      
      connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group","_",i,".rds", sep = "")) 
      Group <- readRDS(connGroup)
      
      GroupList <- append(GroupList, Group)
    }
    new("CollateralGroup",
        Group = GroupList)
    
  }
  
  # 1) construct the tranche factors with the call to new
  
  RDME <- function(Cusip = "character", 
                   PaymentDate = "character", 
                   Coupon = numeric(), 
                   Factor = numeric()){
    new("RDME",
        Cusip = Cusip,
        PaymentDate = PaymentDate,
        Coupon = Coupon,
        Factor = Factor)
  }
    

  
  # 2) serailize tranche factor date to REME directory
  
  MakeRDME <- function(DealName = "character",
                       TrancheNumber = numeric(),
                       Cusip = "character",
                       PaymentDate = "character",
                       Coupon = numeric(),
                       Factor = numeric()){
    
    temp <- RDME(Cusip = Cusip, 
                 PaymentDate = PaymentDate, 
                 Coupon = Coupon, 
                 Factor = Factor)
    
    connRDME <- gzfile(description = paste("~/BondLab/RDME/",DealName,"_","Tranche","_",TrancheNumber,"_","Factor",".rds", sep = ""))
    saveRDS(temp, connRDME)
    close(connRDME)
    
  }
  

  # 3) aggregator function for tranche factor information called by REMIC contructor
  
  RDMEData <- function(NumberofTranches = numeric(), DealName = "character"){
    RDMEList <- list()
    
    for(i in 1 : NumberofTranches){
      
      connRDME <- gzfile(description = paste("~/BondLab/RDME/",DealName,"_","Tranche","_",i,"_","Factor",".rds", sep = "")) 
      RDMEFactor <- readRDS(connRDME)
      
      RDMEList <- append(RDMEList, RDMEFactor)
    }
    new("TrancheFactors",
        FactorData = RDMEList)
    
  }
  

  
 # REMIC constructor -- yeah, baby !!

  
  RemicStructure <- function(DealName = "character"){
    
    #open connection to RAID files and instantiate RAID class
    connRAID <- gzfile(description = paste("~/BondLab/RAID/",DealName,".rds", sep = ""))
    RAID <- readRDS(connRAID)
    #close connection to RAID
    close(connRAID) 
    
    Tranche <- Tranches(NumberofTranches = RAID@NumberofTranches, DealName = RAID@DealName)
    
    CollateralGroup <- CollateralGroup(NumberofGroups = RAID@NumberofGroups, DealName = RAID@DealName)
    FactorData <- RDMEData(NumberofTranches = RAID@NumberofTranches, DealName = RAID@DealName)
    
    REMIC <-new("REMICStructure", 
          
          DealName = RAID@DealName,
          Issuer = RAID@Issuer,
          DealPriceDate = RAID@DealPriceDate,
          DealSettlementDate = RAID@DealSettlementDate,
          Underwriter = RAID@Underwriter,
          NumberofTranches = RAID@NumberofTranches,
          NumberPacSchedules = RAID@NumberPacSchedules,
          NumberofGroups = RAID@NumberofGroups,
          DealSize = RAID@DealSize,
          CollateralAmount = RAID@CollateralAmount,
          Tranches = Tranche@Tranches,
          CollateralGroup = CollateralGroup@Group,
          TrancheFactors = FactorData@FactorData
        )
    
    connREMIC <- gzfile(description = paste("~/BondLab/REMICData/", DealName, ".rds", sep = ""))
    saveRDS(REMIC, connREMIC)
    close(connREMIC)
        
  }
  

