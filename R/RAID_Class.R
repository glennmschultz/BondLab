# --- This class is the remic at issuance disclosure ---
# --- Create class and save to raid file directory ----
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
