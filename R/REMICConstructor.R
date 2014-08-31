# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


# ==== The RAID class is the REMIC at issuance disclosure ===
  setClass("RAID",
  representation(
          DealName = "character", 
          Issuer = "character",
          DealPriceDate = "character",
          DealSettlementDate = "character",
          Underwriter = "character",
          NumberofTranches = "numeric",
          NumberPacSchedules = "numeric",
          NumberofGroups = "numeric",
          DealSize = "numeric",
          CollateralAmount = "numeric"))

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
          }

  )


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
  
  setGeneric("MakeRAID", function(DealName = "character", Issuer = "character", DealPriceDate = "character", DealSettlementDate = "character",
              Underwriter = "character", NumberofTranches = numeric(), NumberPacSchedules = numeric(), NumberofGroups = numeric(), 
              DealSize = numeric(),CollateralAmount = numeric()) {standardGeneric("MakeRAID")})
  # ============== This Class is the Tranche Class Tranche Belongs to Deal =========================
  # ======== This Class contains all Tranche details that are related to the REMIC =================
  
  setClass("TrancheDetails",
           representation(
             DealName = "character",
             TrancheNumber = "character",
             TrancheName = "character",
             TranchePrincipal = "character",
             TrancheInterest = "character",
             Cusip = "character",
             TrancheOrigBal = "numeric",
             TrancheDatedDate = "character",
             TrancheFirstPmtDate = "character",
             TrancheFinalPmtDate = "character",
             TrancheCoupon = "numeric",
             Delay = "numeric",
             PrinPmtFrequency = "numeric",
             InterestPmtFrequency = "numeric",
             FloaterIndex = "character",
             PacLowBand = "numeric",
             PacHighBand = "numeric",
             Group = "numeric"
           ))
  
  setMethod("initialize",
            signature("TrancheDetails"),
            function(.Object,
                     DealName = "character",
                     TrancheNumber = "character",
                     TrancheName = "character",
                     TranchePrincipal = "character",
                     TrancheInterest = "character",
                     Cusip = "character",
                     TrancheOrigBal = numeric(),
                     TrancheDatedDate = "character",
                     TrancheFirstPmtDate = "character",
                     TrancheFinalPmtDate = "character",
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
              .Object@Cusip = Cusip
              .Object@TrancheOrigBal = TrancheOrigBal
              .Object@TrancheDatedDate = TrancheDatedDate
              .Object@TrancheFirstPmtDate = TrancheFirstPmtDate
              .Object@TrancheFinalPmtDate = TrancheFinalPmtDate
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
            }
  )
  
  TrancheDetails <- function( DealName = "character",
                              TrancheNumber = "character",
                              TrancheName = "character",
                              TranchePrincipal = "character",
                              TrancheInterest = "character",
                              Cusip = "character",
                              TrancheOrigBal = numeric(),
                              TrancheDatedDate = "character",
                              TrancheFirstPmtDate = "character",
                              TrancheFinalPmtDate = "character",
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
        Cusip = Cusip,
        TrancheOrigBal = TrancheOrigBal,
        TrancheDatedDate = TrancheDatedDate,
        TrancheFirstPmtDate = TrancheFirstPmtDate,
        TrancheFinalPmtDate = TrancheFinalPmtDate,
        TrancheCoupon = TrancheCoupon,
        Delay = Delay,
        PrinPmtFrequency = PrinPmtFrequency,
        InterestPmtFrequency = InterestPmtFrequency,
        FloaterIndex = FloaterIndex,
        PacLowBand = PacLowBand,
        PacHighBand = PacHighBand,
        Group = Group)
  }
  
  
  
  MakeTranche <- function(  DealName = "character",
                            TrancheNumber = "character",
                            TrancheName = "character",
                            TranchePrincipal = "character",
                            TrancheInterest = "character",
                            Cusip = "character",
                            TrancheOrigBal = numeric(),
                            TrancheDatedDate = "character",
                            TrancheFirstPmtDate = "character",
                            TrancheFinalPmtDate = "character",
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
                            Cusip = Cusip,
                            TrancheOrigBal = TrancheOrigBal,
                            TrancheDatedDate = TrancheDatedDate,
                            TrancheFirstPmtDate = TrancheFirstPmtDate,
                            TrancheFinalPmtDate = TrancheFinalPmtDate,
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
  
  setGeneric("MakeTranche", function(  DealName = "character", TrancheNumber = "character", TrancheName = "character",
                                       TranchePrincipal = "character", TrancheInterest = "character", Cusip = "character",
                                       TrancheOrigBal = numeric(), TrancheDatedDate = "character", TrancheFirstPmtDate = "character",
                                       TrancheFinalPmtDate = "character",TrancheCoupon = numeric(), Delay = numeric(), 
                                       PrinPmtFrequency = numeric(),InterestPmtFrequency = numeric(), FloaterIndex = "character", 
                                       PacLowBand = numeric(), PacHighBand = numeric(), Group = numeric())
                                       {standardGeneric("MakeTranche")})
  
  # ======== Tranches class is an aggregator calss for all tranches related to a REMIC ======
  # ================== The Tranches function is called in the REMIC Constructor =============
  
  # The function assembles multiple tranches associated with a deal 
  # building the tranche classes into a list
  
  setClass("Tranches",
           representation(
             Tranches = "list"))
  
  setMethod("initialize",
            signature("Tranches"),
            function(.Object,...,
                     Tranches = list())
            {
              .Object@Tranches = Tranches
              
              return(.Object)
              
              callNextMethod(.Object, ...)
            }
  )
  
  
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
  
  setGeneric("Tranches", function(NumberofTranches = numeric(), DealName = "character") {standardGeneric("Tranches")})
  
  # ========= Collateral Class is the collateral group backing the REMIC ==========
  
  setClass("Collateral",
           representation(
             Group = "numeric",
             Cusip = "list",
             OrigBal = "list"))
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
            }
  )
  
  Collateral <- function(DealName = "character", 
                         Group = numeric(), 
                         Cusip = list(), 
                         OrigBal = list()){
    new("Collateral",
        Group = as.numeric(Group),
        Cusip = as.list(Cusip),
        OrigBal = as.list(OrigBal)
    )
  }
  
  setGeneric("Collateral", function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list())
  {standardGeneric("Collateral")})
  
  
  # ================= Function to make the collateral rds file =================
  # This is the input function for collateral groups
  MakeCollateral <- function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list()){
    
    temp <- Collateral(DealName = DealName, Group = Group, Cusip = Cusip, OrigBal = OrigBal)
    
    connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group","_",temp@Group,".rds", sep = ""))
    saveRDS(temp, connGroup)
    
  }
  setGeneric("MakeCollateral", function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list())
    {standardGeneric("MakeCollateral")})
  
  # ========== Collateral Group Class is an aggregator of the collateral class ================
  # This function assembles multiple collateral groups into a list of collateral groups
  # building the collateral groups for the entire deal structure
  
  setClass("CollateralGroup",
           representation(
             Group = "list"))
  
  setMethod("initialize",
            signature("CollateralGroup"),
            function (.Object,
                      Group = list()) 
            {
              .Object@Group = Group
              return(.Object)
              
              callNextMethod(.Object,...)  
            }
  )
  
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
  
  setGeneric("CollateralGroup", function(NumberofGroups = numeric(), DealName = "character") {standardGeneric("CollateralGroup")})

  # ======== This calss is the REMIC factor files and belongs to tranche information ==================
  # REMIC Disclosure Month End (RDME) Class stores the tranch factor data and is part of the assembly of the REMIC
    
  setClass("RDME",
           representation(
             Cusip = "character",
             PaymentDate = "character",
             Coupon = "numeric",
             Factor = "numeric"))
  
  setMethod("initialize",
            signature("RDME"),
            function(.Object,
                      Cusip = "character",
                      PaymentDate = "charcter",
                      Coupon = numeric(),
                      Factor = numeric())
            {
              .Object@Cusip = Cusip
              .Object@PaymentDate = PaymentDate
              .Object@Coupon = Coupon
              .Object@Factor = Factor
              
              return(.Object)
              
              callNextMethod(.Object,...)  
              
            }
                    )
  
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
    
  setGeneric("RDME", function(Cusip = "character", PaymentDate = "character", Coupon = numeric(), Factor = numeric())
    {standardGeneric("RDME")})  
  
  # =========================== This is the input function for the  RDME rds file ===================== 
  
  MakeRDME <- function(DealName = "character",
                       TrancheNumber = numeric(),
                       Cusip = "character",
                       PaymentDate = "character",
                       Coupon = numeric(),
                       Factor = numeric()){
    
    temp <- RDME(Cusip = Cusip, PaymentDate = "character", Coupon = Coupon, Factor = Factor)
    connRDME <- gzfile(description = paste("~/BondLab/RDME/",DealName,"_","Tranche","_",TrancheNumber,"_","Factor",".rds", sep = ""))
    saveRDS(temp, connRDME)
    close(connRDME)
    
  }
  
  setGeneric("MakeRDME", function(DealName = "character", TrancheNumber = numeric(), Cusip = "character", 
                                  PaymentDate = "character", Coupon = numeric(), Factor = numeric())
                                  {standardGeneric("MakeRDME")})
    
  
  # =============== The TrancheFactors class is an aggregator class ===================
  # ============ The class aggregates the RDME classes for each associated trance ====
  
  setClass("TrancheFactors",
           representation(
             FactorData = "list"))
  
  setMethod("initialize",
            signature("TrancheFactors"),
            function(.Object,
                     FactorData = list())
                    {
                     .Object@FactorData = FactorData
                     return(.Object)
                     
                     callNextMethod(.Object,...)  
                     }
            )
  
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
  
  setGeneric("RDMEData", function(NumberofTranches = numeric(), DealName = "character") {standardGeneric("RDMEData")})
  
  #========== Superclass REMIC structure constructor for REMIC which will be called by the waterfall ==========
  setClass("REMICStructure",
           representation(),
           contains = c("RAID", "Tranches", "CollateralGroup", "TrancheFactors")) 
  
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
  }
  )
  
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
  
  setGeneric("REMICStructure", function(DealName = "character") {standardGeneric("REMICStructure")})
