# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
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
                   FloaterMargin = numeric(),
                   FloaterCap = numeric(),
                   FloaterFloor = numeric(),
                   FloaterFormula = "function",
                   PacLowBand = numeric(),
                   PacHighBand = numeric(),
                   Group = numeric(),
                   Schedule = "logical",
                   Fixed = "logical"){
            
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
            .Object@FloaterIndex = FloaterIndex      
            .Object@FloaterMargin = FloaterMargin
            .Object@FloaterCap = FloaterCap
            .Object@FloaterFloor = FloaterFloor
            .Object@FloaterFormula = FloaterFormula
            .Object@PacLowBand = PacLowBand
            .Object@PacHighBand = PacHighBand
            .Object@Group = Group
            .Object@Schedule = Schedule
            .Object@Fixed = Fixed
            
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

# Initialize Schedule
setMethod("initialize",
          signature("Schedule"),
          function (.Object,
                    DealName = "character",
                    Group = numeric(),
                    PmtDate = "character",
                    Balance = numeric(),
                    ScheduledPmt = numeric()){
                      
                      .Object@DealName = DealName
                      .Object@Group = Group
                      .Object@PmtDate = PmtDate
                      .Object@Balance = Balance
                      .Object@ScheduledPmt = ScheduledPmt
                      
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
#'@export
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
#'@export
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
  
  #------- Tranches functions for the REMIC constructor ---------------------------------------------------------------
#Construct Tranche details
#'@export
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
                              FloaterMargin = numeric(),
                              FloaterCap = numeric(),
                              FloaterFloor = numeric(),
                              FloaterFormula = "function",
                              PacLowBand = numeric(),
                              PacHighBand = numeric(),
                              Group = numeric(),
                              Schedule = "logical",
                              Fixed = "logical"){
    
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
        FloaterMargin = FloaterMargin,
        FloaterCap = FloaterCap,
        FloaterFloor = FloaterFloor,
        FloaterFormula = FloaterFormula,
        PacLowBand = PacLowBand,
        PacHighBand = PacHighBand,
        Group = Group,
        Schedule = Schedule,
        Fixed = Fixed)
  }
  
  
# 2) serialize the tranches to the tranches directory
#'@export
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
                            FloaterMargin = numeric(),
                            FloaterCap = numeric(),
                            FloaterFloor = numeric(),
                            FloaterFormula = "function",
                            PacLowBand = numeric(),
                            PacHighBand = numeric(),
                            Group = numeric(),
                            Schedule = "logical",
                            Fixed = "logical") {
    
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
                            FloaterMargin = FloaterMargin,
                            FloaterCap = FloaterCap,
                            FloaterFloor = FloaterFloor,
                            FloaterFormula = FloaterFormula,
                            PacLowBand = PacLowBand,
                            PacHighBand = PacHighBand,
                            Group = Group,
                            Schedule = Schedule,
                            Fixed = Fixed)
    
    connTranche <- gzfile(description = paste("~/BondLab/Tranches/",DealName,"_","Tranche","_",temp@TrancheNumber,".rds", sep = ""))
    saveRDS(temp, connTranche)
    close(connTranche)
  }

  # 3) tranches assembles the tranches for REMIC structure and is called by REMIC constructor function
  # The function assembles multiple tranches associated with a deal 
  # building the tranche classes into a list
#'@export  
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

  # -------- REMIC Schedules PAC and TAC schedules for REMIC
  #1 construct the PAC REMIC Class with call to new
  #' @importFrom lubridate %m+%
  #' @export
  Schedule <- function(bond.id = "character",
                             DealName = "character",
                             Group = "character",
                             original.bal = numeric(),
                             trade.date = "character",
                             settlement.date = "character",
                             first.pmtdate = "character",
                             price = numeric(),
                             begin.cpr = numeric(),
                             end.cpr = numeric(),
                             seasoning.period = numeric(),
                             lower.PSA = numeric(), 
                             upper.PSA = numeric()){
  
  if(missing(lower.PSA)) stop ("Missing Lower PSA")
  if(missing(upper.PSA)) stop ("Missing Upper PSA")
  
  if(lower.PSA < 10) stop ("Lower PSA must be in Percentage")
  if(upper.PSA < 10) stop ("Upper PSA must be in Percentage")
  
  # ---- connect to the bond data folder
  bond.id <- MBS(MBS.id = bond.id)
  # ---- connect to rates data folder
  
  rates.data <- Rates(trade.date = trade.date)
  
  # --- connect to mortgage rate model
  MortgageRate <- MtgRate()
  
  # --- connect to the prepayment model
  ModelTune <- ModelTune(bond.id = bond.id)
  
  TermStructure <- TermStructure(rates.data = rates.data, method = "ns")
  Burnout <- bond.id@Burnout
  
  PSA.Band <- c(lower.PSA/100, upper.PSA/100)
  Principal <- list()
  
  for(i in 1 : 2){
    
    begin.cpr <- begin.cpr * PSA.Band[i]
    end.cpr <- end.cpr * PSA.Band[i]
    
    PrepaymentAssumption <- PrepaymentAssumption(bond.id = bond.id,
                                                 MortgageRate = MortgageRate,
                                                 TermStructure = TermStructure,
                                                 PrepaymentAssumption = "PPC",
                                                 ModelTune = ModelTune,
                                                 Burnout = Burnout,
                                                 begin.cpr = begin.cpr,
                                                 end.cpr = end.cpr,
                                                 seasoning.period = seasoning.period
    )
    
    MortgageCashFlow <-  MortgageCashFlow(bond.id = bond.id,
                                          original.bal = original.bal,    
                                          settlement.date = settlement.date,
                                          price = price,
                                          PrepaymentAssumption = PrepaymentAssumption)
    
    Principal[[i]] <-MortgageCashFlow@ScheduledPrin + MortgageCashFlow@PrepaidPrin

  }
  Matrix <- do.call(cbind,Principal)
  colnames(Matrix) <- c(paste(lower.PSA, "PSA", sep = ""),paste(upper.PSA, "PSA", sep = "") )
  PACSched <- apply(Matrix, 1, min)
  PACBal <- sum(PACSched)
  PmtDate <- as.Date(first.pmtdate, "%m-%d-%Y") %m+% months(0:360)
  
  new("Schedule",
      DealName = DealName,
      Group = Group,
      PmtDate = as.character(PmtDate),
      Balance = as.numeric(unname(PACBal - cumsum(PACSched))),
      ScheduledPmt = as.numeric(unname(PACSched)))

}
  
  # ---------- function to create and save the PAC schedule class ----------------------------------------------------
  #' MakeSchedule
  #' 
  #' Function to create a PAC bond sinking fund schedule
  #' @param bond.id A character string the cusip or id
  #' @param DealName A character string the transaction deal name
  #' @param Group A character string the tranche's collateral group
  #' @param original.balance A numeric value the collateral group original balance
  #' @param trade.date A character string the trade date
  #' @param settlement.date A character string the settlement date
  #' @param first.pmtdate A character string the bond first payment date
  #' @param price A numeric value the price of the underlying collateral
  #' @param begin.cpr A numeric value the beginning value of the PPC assumption
  #' @param end.cpr A numeric value the ending value of the PPC assumption
  #' @param seasoning.period A numeric value the length of the PPC ramp
  #' @param lower.PSA A numeric value the lower PSA band
  #' @param upper.PSA A numeric value the upper PSA band
  #' @examples MakeSchedule(bond.id = "BondLabMBS4",DealName = "BondLabPAC01",Group = 1,
  #' original.bal = 200000000,first.pmtdate = "01-25-2013",trade.date = "01-10-2013",
  #' settlement.date = "01-13-2013",price = 105.75,begin.cpr = .2,end.cpr = 6,
  #' seasoning.period = 30,lower.PSA = 75, upper.PSA = 250)
  #' @export  
  MakeSchedule <- function(bond.id = "character",
                         DealName = "character",
                         Group = "character",
                         original.bal = numeric(),
                         trade.date = "character",
                         settlement.date = "character",
                         first.pmtdate = "character",
                         price = numeric(),
                         begin.cpr = numeric(),
                         end.cpr = numeric(),
                         seasoning.period = numeric(),
                         lower.PSA = numeric(), 
                         upper.PSA = numeric()){
  
                         temp <- Schedule(bond.id = bond.id,
                                          DealName = DealName,
                                          Group = Group,
                                          original.bal = original.bal,
                                          trade.date = trade.date,
                                          settlement.date = settlement.date,
                                          first.pmtdate = first.pmtdate,
                                          price = price,
                                          begin.cpr = begin.cpr,
                                          end.cpr = end.cpr,
                                          seasoning.period = seasoning.period,
                                          lower.PSA = lower.PSA,
                                          upper.PSA = upper.PSA
                                          )
                         connSched <- gzfile(description = paste("~/BondLab/Schedules/",
                                      DealName,"_","Group","_",temp@Group,"_", "Sch", ".rds", sep = ""))
                         saveRDS(temp, connSched)
}



  # -------- Collateral groups for the REMIC Constructor -------------------------------------------------------------
  # 1) construct the collateral class with call to new
  #'@export  
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
  #'@export
  MakeCollateral <- function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list()){
    
    temp <- Collateral(DealName = DealName, Group = Group, Cusip = Cusip, OrigBal = OrigBal)
    
    connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group","_",temp@Group,".rds", sep = ""))
    saveRDS(temp, connGroup)
    
  }

  # 3) aggregator function for the REMIC structure called by REMIC constructor
  # the function assembles multiple collateral groups can be extended to loan level
  #'@export
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
  # ------ RDME Functions for the REMIC structuring tool ---------------------------------------------------------
  # 1) construct the tranche factors with the call to new
  #'@export
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
    

  
  # 2) serailize tranche factor date to RDME directory
  #'@export
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
  #'@export
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
  
  #4) REMIC Constructor
#'@export
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
  

