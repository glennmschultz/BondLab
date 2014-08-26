# --- This class is the REMIC at issuance disclosure ---
  setClass("RAID",
  representation(
          DealName = "character", 
          Issuer = "character",
          DealPriceDate = "character",
          DealSettlementDate = "character",
          Underwriter = "character",
          NumberofTranches = "numeric",
          NumberPacSchedules = "numeric",
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
              .Object@DealSize = DealSize
              .Object@CollateralAmount = CollateralAmount
 
              return(.Object) 
              callNextMethod()               
          }

  )

  #Constructor function for the RAID Class

  RAID <- function(DealName = "character",
                 Issuer = "character",
                 DealPriceDate = "character",
                 DealSettlementDate = "character",
                 Underwriter = "character",
                 NumberofTranches = numeric(),
                 NumberPacSchedules = numeric(),
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
                     DealSize = DealSize,
                     CollateralAmount = CollateralAmount
                     )                 
          }

  # function calls the RAID
  MakeRAID <- function(DealName = "character", 
                     Issuer = "character", 
                     DealPriceDate = "character", 
                     DealSettlementDate = "character",
                     Underwriter = "character",
                     NumberofTranches = numeric(),
                     NumberPacSchedules = numeric(),
                     DealSize = numeric(),
                     CollateralAmount = numeric()){
  
  temp <-RAID(DealName = DealName,
              Issuer = Issuer,
              DealPriceDate = DealPriceDate,
              DealSettlementDate = DealSettlementDate,
              Underwriter = Underwriter,
              NumberofTranches = NumberofTranches,
              NumberPacSchedules = NumberPacSchedules,
              DealSize = DealSize,
              CollateralAmount = CollateralAmount)
  connRAID <- gzfile(description = paste("~/BondLab/RAID/",temp@DealName,".rds", sep = ""))
  saveRDS(temp, connRAID)
  close(connRAID)
  }
    
  # ============== This Class is the Tranche Class Tranche Belongs to Deal =========================
  setClass("TrancheDetails",
           representation(
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
            }
  )
  
  TrancheDetails <- function( TrancheNumber = "character",
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
    
    temp <- TrancheDetails( TrancheNumber = TrancheNumber,
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
  
  # ======== Tranches class is a list of tranches for the REMIC ===
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
  
  # ========= This Class is the Collateral Class ==========
  
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
              return(.Object) }
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
  
  
  MakeCollateral <- function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list()){
    
    temp <- Collateral(DealName = DealName, Group = Group, Cusip = Cusip, OrigBal = OrigBal)
    
    connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group",temp@Group,".rds", sep = ""))
    saveRDS(temp, connGroup)
    
  }
  
  # ========== This Class is Collateral Group Collateral Groups Belong to Tranches ================
  
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
            }
  )
  
  CollateralGroup <- function(NumberofGroups = numeric(), DealName = "character"){
    
    GroupList <- list()
    
    for(i in 1 : NumberofGroups){
      
      connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group",i,".rds", sep = "")) 
      Group <- readRDS(connGroup)
      
      GroupList <- append(GroupList, Group)
    }
    new("CollateralGroup",
        Group = GroupList)
    
    #close(connGroup)
  }
  # REMIC Disclosure Month End (RDME) Class stores the tranch factor data and is part of the assembly of the REMIC
  #  
  setClass("RDME",
           representation(
             Cusip = "character",
             PaymentDate = "character",
             Coupon = "numeric",
             Factor = "numeric"))
  
  setMethod("initialize",
            signature("RMDE"),
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
  
  setClass("REMICStructure",
            contains = c("RAID", "Tranches")) 
  
  setMethod('initialize',
            signature("REMICStructure"),
             function(.Object,...)
               {
             return(.Object)
             callNextMethod()
            
  }
  )
  
  RemicStructure <- function(DealName = "character"){
    
    #open connection to RAID files and instantiate RAID class
    connRAID <- gzfile(description = paste("~/BondLab/RAID/",DealName,".rds", sep = ""))
    RAID <- readRDS(connRAID)
    #close connection to RAID
    #close(connRAID) 
    
    Tranche <<- Tranches(NumberofTranches = RAID@NumberofTranches, DealName = RAID@DealName)
    
    #CollateralGroup <- CollateralGroup(NumberofGroups = 1, DealName = Deal@DealName)
    
    #FactorData <- RDMEData(NumberofTranches = Deal@NumberofTranches, DealName = Deal@DealName)
    
    new("REMICStructure", 
        DealName = RAID@DealName, 
        Issuer = RAID@Issuer,
        Tranche = Tranche@Tranches)


  }
  
  
