RAID <- setMethod("initialize", signature (.Object = "RAID"),
          function(.Object, DealName = "character",
                   Issuer = "character", 
                   DealPriceDate = "character",
                   DealSettlementDate = "character",
                   Underwriter = "character",
                   NumberofTranches = "character",
                   PacSchedules = "character",
                   NumberPacSchedules = "character",
                   DealSize = numeric(),
                   CollateralAmount = numeric(),
                   Groups = "list",
                   Tranches = "list"){
        
      RAID <-  new("RAID",
        DealName = DealName,
        Issuer = Issuer,
        DealPriceDate = DealPriceDate,
        DealSettlementDate = DealSettlementDate,
        Underwriter = Underwriter,
        NumberofTranches = NumberofTranches,
        PacSchedules = PacSchedules,
        NumberPacSchedules = NumberPacSchedules,
        DealSize = DealSize,
        CollateralAmount = CollateralAmount,
        Groups = Groups,
        Tranches = Tranches
        )

  connRAID <- gzfile(description = paste("~/BondLab/REMICData/", RAID@DealName, ".rds", sep = ""))
  saveRDS(connRAID)

  close(connRAID)

}
)

# --- This class is the remic at issuance disclosure ---
setClass("RAID",
         representation(
          DealName = "character", 
          Issuer = "character",
          DealPriceDate = "character",
          DealSettlementDate = "character",
          Underwriter = "character",
          NumberofTranches = "numeric",
          PacSchedules = "character",
          NumberPacSchedules = "numeric",
          DealSize = "numeric",
          CollateralAmount = "numeric",
          Groups = "list",
          Tranches = "list"))

setGeneric("RAID",  function(DealName = "character",
                             Issuer = "character", 
                             DealPriceDate = "character",
                             DealSettlementDate = "character",
                             Underwriter = "character",
                             NumberofTranches = "character",
                             PacSchedules = "character",
                             NumberPacSchedules = "character",
                             DealSize = numeric(),
                             CollateralAmount = numeric(),
                             Groups = "list",
                             Tranches = "list")
{standardGeneric("RAID")})