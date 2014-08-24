# --- This class is the REMIC at issuance disclosure ---
  RAID <- setClass("RAID",
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
                    NumberofTranches = "numeric",
                    NumberPacSchedules = "numeric",
                    DealSize = "numeric",
                    CollateralAmount = "numeric"
                    )
  {
            if(missing(DealName))
              stop("Missing Deal Name")
            if(missing(Issuer))
              stop("Missing Issuer")
            if(missing(DealPriceDate))
              stop("Missing Deal Price Date")
            if(missing(DealPriceDate))
              stop("Invalid Price Date")
            if(missing(DealSettlementDate))
              stop("Missing Deal Settlement Date")
            if(missing(DealSettlementDate))
              stop("Invalid Settlement Date")
            if(missing(Underwriter))
              stop("Missing Underwriter")
            if(missing(NumberofTranches))
              stop("Missing Tranches")
            if(missing(NumberPacSchedules))
              stop("Missing Number Pac Schedules")
            if(missing(DealSize))
              stop("Missing Deal Size")
            if(missing(CollateralAmount))
              stop("Missing Collateral Amount")
              
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
  }
  
# ============== This Class is the Tranche Class Tranche Belongs to Deal =========================
  TrancheDetails <- setClass("TrancheDetails",
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
                     TrancheOrigBal = "numeric",
                     TrancheDatedDate = "character",
                     TrancheFirstPmtDate = "character",
                     TrancheFinalPmtDate = "character",
                     Delay = "numeric",
                     PrinPmtFrequency = "numeric",
                     InterestPmtFrequency = "numeric",
                     FloaterIndex = "character",
                     PacLowBand = "numeric",
                     PacHighBand = "numeric",
                     Group = "numeric"){
            
              .Object@TrancheNumber = TrancheNumber
              .Object@TrancheName = TrancheName
              .Object@TranchePrincipal = TranchePrincipal
              .Object@TrancheInterest = TrancheInterest
              .Object@Cusip = Cusip
              .Object@TrancheOrigBal = OriginalBal
              .Object@TrancheDatedDate = TrancheDate
              .Object@TrancheFirstPmtDate = TrancheFirstPmtDate
              .Object@TrancheFinalPmtDate = TrancheFinalPmtDate
              .Object@Delay = Delay
              .Object@PrinPmtFrequency = PrinPmtFrequency
              .Object@InterestPmtFrequency
              .Object@FloaterIndex
              .Object@PacLowBand
              .Object@PacHighBand
              .Object@Group
                            
            }
            )
  
  TrancheDetails <- function( TrancheNumber = "character",
                              TrancheName = "character",
                              TranchePrincipal = "character",
                              TrancheInterest = "character",
                              Cusip = "character",
                              TrancheOrigBal = "numeric",
                              TrancheDatedDate = "character",
                              TrancheFirstPmtDate = "character",
                              TrancheFinalPmtDate = "character",
                              Delay = "numeric",
                              PrinPmtFrequency = "numeric",
                              InterestPmtFrequency = "numeric",
                              FloaterIndex = "character",
                              PacLowBand = "numeric",
                              PacHighBand = "numeric",
                              Group = "number"){
    
                      new("TrancheDetails",
                          TrancheNumber = TrancheNumber,
                          TrancheName = TrancheName,
                          TranchePrincipal = TranchePrincipal,
                          TrancheInterest = TrancheInterest,
                          Cusip = Cusip,
                          TrancheOrigBal = TrancheOrigBal,
                          TrancheDatedDate = TrancheDateDate,
                          TrancheFirstPmtDate = TrancheFirstPmtDate,
                          TrancheFinalPmtdate = TrancheFinalPmtDate,
                          Delay = Delay,
                          PrinPmtFrequency = PrinPmtFrequency,
                          InterestPmtFrequency = InterestPmtFrequency,
                          FloaterIndex,
                          PacLowerBand,
                          PacHigherBand,
                          Group = Group)
  }


  
# ========= This Class is the Collateral Class ==========
  
  Collateral <- setClass("Collateral",
                         representation(
                           Group = "numeric",
                           Cusip = "list",
                           OrigBal = "list"))
  setMethod("initialize",
            signature ("Collateral"),
            function (.Object,
                      Group = "numeric",
                      Cusip = "list",
                      OrigBal = "list"){
              
              if(missing(Group))
                stop("Missing Group Number")
              if(missing(Cusip))
              stop("Missing Cusip Details")
              if(missing(OrigBal))
                stop("Missing Orig Bal")
              if(length(Cusip) != length(OrigBal))
                stop("Cusip and Orig Balance not assigned correctly")
            
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
      
    connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_",Group,".rds", sep = ""))
    saveRDS(temp, connGroup)
    
  }
  
  # ========== This Class is Collateral Group Collateral Groups Belong to Tranches ================
  
  # This function needs to be able to assemble multiple collateral groups into a list of collateral groups
  # building the collateral groups for the entire deal structure
  
  CollateralGroup <- setClass("CollateralGroup",
                              representation(
                                Group = "list"))
  
  setMethod("initialize",
            signature("CollateralGroup"),
            function (.Object,
                      Group = "list") 
            {
              .Object@Group = Group
              return(.Object)
            }
  )
  
  CollateralGroup <- function(NumberGroups =numeric(), DealName = "character"){
    
    GroupList <- list()
    
    for(i in 1 : NumberGroups){
      
      connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_",i,".rds", sep = "")) 
      Group <- readRDS(connGroup)
      
      GroupList <- append(GroupList, Group)
    }
    new("CollateralGroup",
        Group = GroupList)
  }
  
  