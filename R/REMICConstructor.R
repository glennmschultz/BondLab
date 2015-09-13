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
                    DealNumber = "character",
                    DealPriceDate = "character",
                    DealSettlementDate = "character",
                    Underwriter = "character",
                    Trustee = "character",
                    PricingSpeed = numeric(),
                    JumpReferenceSpeed1 = numeric(),
                    JumpReferenceSpeed2 = numeric(),
                    JumpReferenceSpeed3 = numeric(),
                    JumpReferenceSpeed4 = numeric(),
                    NumberofTranches = numeric(),
                    NumberofComponentTranches = numeric(),
                    NumberofCombinedTranches = numeric(),
                    NumberofPools = numeric(),
                    PacSchedulesIncluded = "logical",
                    NumberofPacSchedules = numeric(),
                    NumberofGroups = numeric(),
                    DealSize = numeric(),
                    CollateralAmount = numeric(),
                    CollateralAvgLife = numeric(),
                    BondValue = numeric(),
                    BondValueMethod = "character",
                    BondValueCap = numeric(),
                    BondValueDiscountRate = numeric(),
                    BondValueReinvestmentRate = numeric(),
                    ExpenseBasisPointFee = numeric(),
                    ExpenseFixed = numeric(),
                    ExpensePeriodicity = numeric(),
                    InitialReserveFund = numeric())
          {
            
            .Object@DealName = DealName
            .Object@Issuer = Issuer
            .Object@DealNumber = DealNumber
            .Object@DealPriceDate = DealPriceDate
            .Object@DealSettlementDate = DealSettlementDate
            .Object@Underwriter = Underwriter
            .Object@Trustee = Trustee
            .Object@PricingSpeed = PricingSpeed
            .Object@JumpReferenceSpeed1 = JumpReferenceSpeed1
            .Object@JumpReferenceSpeed2 = JumpReferenceSpeed2
            .Object@JumpReferenceSpeed3 = JumpReferenceSpeed3
            .Object@JumpReferenceSpeed4 = JumpReferenceSpeed4
            .Object@NumberofTranches = NumberofTranches
            .Object@NumberofComponentTranches = NumberofComponentTranches
            .Object@NumberofCombinedTranches = NumberofCombinedTranches
            .Object@NumberofPools = NumberofPools
            .Object@PacSchedulesIncluded = PacSchedulesIncluded
            .Object@NumberofGroups = NumberofGroups
            .Object@NumberofPacSchedules = NumberofPacSchedules
            .Object@NumberofGroups = NumberofGroups
            .Object@DealSize = DealSize
            .Object@CollateralAmount = CollateralAmount
            .Object@CollateralAvgLife = CollateralAvgLife
            .Object@BondValue = BondValue
            .Object@BondValueMethod = BondValueMethod
            .Object@BondValueCap = BondValueCap
            .Object@BondValueDiscountRate = BondValueDiscountRate
            .Object@BondValueReinvestmentRate = BondValueReinvestmentRate
            .Object@ExpenseBasisPointFee = ExpenseBasisPointFee
            .Object@ExpenseFixed = ExpenseFixed
            .Object@ExpensePeriodicity = ExpensePeriodicity
            .Object@InitialReserveFund = InitialReserveFund
            
            return(.Object) 
            callNextMethod(.Object,...)               
          })

  # Initialize TrancheDetails
  setMethod("initialize",
          signature("TrancheDetails"),
          function(.Object,
                   DealName = "character",
                   TrancheNumber = "character",
                   NumberofComponents = numeric(),
                   ComponentofTranches = "character",
                   TrancheName = "character",
                   TranchePrincipal = "character",
                   TranchePrincipalDesc = "character",
                   TrancheInterestDesc = "character",     
                   TrancheOtherDescription = "character",
                   Cusip = "character",
                   TrancheOrigBal = numeric(),
                   TrancheInterest = "character",
                   TrancheCoupon = numeric(),
                   AccrualRate = numeric(),
                   TreasuryMaturity = numeric(),
                   TreasuryYield = numeric(),
                   TreasurySpread = numeric(),
                   TrancheYield = numeric(),
                   TranchePrice = mumeric(),
                   TrancheProceedsWithInterest = numeric(),
                   TrancheAvgLife = numeric(),
                   TrancheDuration = numeric(),
                   TrancheDatedDate = numeric(),
                   TrancheFirstPmtDate = "character",
                   TrancheLastPmtDate = "character",
                   TrancheNextPmtDate = "character",
                   TrancheFinalPmtDate = "character",
                   Delay = numeric(),
                   InterestPmtFrequency = numeric(),
                   PrinPmtFrequency = numeric(),
                   PacLowBand = numeric(),
                   PacHighBand = numeric(),
                   FloaterIndex = "character",
                   InitialIndexValue = numeric(),
                   FloaterMargin = numeric(),
                   FloaterMultiplier = numeric(),
                   FloaterCap = numeric(),
                   FloaterFloor = numeric(),
                   FloaterInitialCoupon = numeric(),
                   FloaterResetFrequency = numeric(),
                   FloaterFirstResetDate = "character",
                   FloaterFormula = "function",
                   Group = numeric(),
                   TrancheType = "character",
                   Schedule = "logical",
                   Fixed = "logical"){
            
            .Object@DealName = DealName
            .Object@TrancheNumber = TrancheNumber
            .Object@NumberofComponents = NumberofComponents
            .Object@ComponentofTranches = ComponentofTranches
            .Object@TrancheName = TrancheName
            .Object@TranchePrincipal = TranchePrincipal
            .Object@TranchePrincipalDesc = TranchePrincipalDesc
            .Object@TrancheInterestDesc = TrancheInterestDesc     
            .Object@TrancheOtherDescription = TrancheOtherDescription
            .Object@Cusip = Cusip
            .Object@TrancheOrigBal = TrancheOrigBal
            .Object@TrancheInterest = TrancheInterest
            .Object@TrancheCoupon = TrancheCoupon
            .Object@AccrualRate = AccrualRate
            .Object@TreasuryMaturity = TreasuryMaturity
            .Object@TreasuryYield = TreasuryYield
            .Object@TreasurySpread = TreasurySpread
            .Object@TrancheYield = TreasuryYield
            .Object@TranchePrice = TranchePrice
            .Object@TrancheProceedsWithInterest = TrancheProceedsWithInterest
            .Object@TrancheAvgLife = TrancheAvgLife
            .Object@TrancheDuration = TrancheDuration
            .Object@TrancheDatedDate = TrancheDatedDate
            .Object@TrancheFirstPmtDate = TrancheFirstPmtDate
            .Object@TrancheLastPmtDate = TrancheLastPmtDate
            .Object@TrancheNextPmtDate = TrancheNextPmtDate
            .Object@TrancheFinalPmtDate = TrancheFinalPmtDate
            .Object@Delay = Delay
            .Object@InterestPmtFrequency = InterestPmtFrequency
            .Object@PrinPmtFrequency = PrinPmtFrequency
            .Object@PacLowBand = PacLowBand
            .Object@PacHighBand = PacHighBand
            .Object@FloaterIndex = FloaterIndex
            .Object@InitialIndexValue = InitialIndexValue
            .Object@FloaterMargin = FloaterMargin
            .Object@FloaterMultiplier = FloaterMultiplier
            .Object@FloaterCap = FloaterCap
            .Object@FloaterFloor = FloaterFloor
            .Object@FloaterInitialCoupon = FloaterInitialCoupon
            .Object@FloaterResetFrequency = FloaterResetFrequency
            .Object@FloaterFirstResetDate = FloaterFirstResetDate
            .Object@FloaterFormula = FloaterFormula
            .Object@Group = Group
            .Object@TrancheType = TrancheType
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
                   DealNumber = "character",
                   DealPriceDate = "character",
                   DealSettlementDate = "character",
                   Underwriter = "character",
                   Trustee = "character",
                   PricingSpeed = "numeric",
                   JumpReferenceSpeed1 = "numeric",
                   JumpReferenceSpeed2 = "numeric",
                   JumpReferenceSpeed3 = "numeric",
                   JumpReferenceSpeed4 = "numeric",
                   NumberofTranches = "numeric",
                   NumberofComponentTranches = "numeric",
                   NumberofCombinedTranches = "numeric",
                   NumberofPools = "numeric",
                   PacSchedulesIncluded = "logical",
                   NumberofPacSchedules = numeric(),
                   NumberofGroups = numeric(),                   
                   DealSize = numeric(),                   
                   CollateralAmount = numeric(),
                   BondValue = "numeric",
                   BondValueMethod = "character",
                   BondValueCap = "numeric",
                   BondValueDiscountRate = "numeric",
                   BondValueReinvestmentRate = "numeric",
                   ExpenseBasisPointFee = "numeric",
                   ExpenseFixed = "numeric",
                   ExpensePeriodicity = "numeric",
                   InitialReserveFund = "numeric",
                   Tranches = "character",
                   CollateralGroup = "character",
                   TrancheFactors ="character")
        {
                    .Object@DealName = DealName
                    .Object@Issuer = Issuer
                    .Object@DealNumber = DealNumber
                    .Object@DealPriceDate = DealPriceDate
                    .Object@DealSettlementDate = DealSettlementDate
                    .Object@Underwriter = Underwriter 
                    .Object@Trustee = Trustee
                    .Object@PricingSpeed = PricingSpeed
                    .Object@JumpReferenceSpeed1 = JumpReferenceSpeed1
                    .Object@JumpReferenceSpeed2 = JumpReferenceSpeed2
                    .Object@JumpReferenceSpeed3 = JumpReferenceSpeed3
                    .Object@JumpReferenceSpeed4 = JumpReferenceSpeed4
                    .Object@NumberofTranches = NumberofTranches
                    .Object@NumberofComponentTranches = NumberofComponentTranches
                    .Object@NumberofCombinedTranches = NumberofCombinedTranches
                    .Object@NumberofPools = NumberofPools
                    .Object@PacSchedulesIncluded = PacSchedulesIncluded
                    .Object@NumberofPacSchedules = NumberofPacSchedules
                    .Object@NumberofGroups = NumberofGroups                   
                    .Object@DealSize = DealSize                   
                    .Object@CollateralAmount = CollateralAmount
                    .Object@BondValue = BondValue
                    .Object@BondValueMethod = BondValueMethod
                    .Object@BondValueCap = BondValueCap
                    .Object@BondValueDiscountRate = BondValueDiscountRate
                    .Object@BondValueReinvestmentRate = BondValueReinvestmentRate
                    .Object@ExpenseBasisPointFee = ExpenseBasisPointFee
                    .Object@ExpenseFixed = ExpenseFixed
                    .Object@ExpensePeriodicity = ExpensePeriodicity
                    .Object@InitialReserveFund = InitialReserveFund
                    .Object@Tranches = Tranches
                    .Object@Group = CollateralGroup
                    .Object@FactorData = TrancheFactors
            return(.Object)
            
            callNextMethod(.Object,...)           
          })


  RAID <- function(DealName = "character",
                 Issuer = "character",
                 DealNumber = "character",
                 DealPriceDate = "character",
                 DealSettlementDate = "character",
                 Underwriter = "character",
                 Trustee = "character",
                 PricingSpeed = numeric(),
                 JumpReferenceSpeed1 = numeric(),
                 JumpReferenceSpeed2 = numeric(),
                 JumpReferenceSpeed3 = numeric(),
                 JumpReferenceSpeed4 = numeric(),
                 NumberofTranches = numeric(),
                 NumberofComponentTranches = numeric(),
                 NumberofCombinedTranches = numeric(),
                 NumberofPools = numeric(),
                 PacSchedulesIncluded = "logical",
                 NumberofPacSchedules = numeric(),
                 NumberofGroups = numeric(),
                 DealSize = numeric(),
                 CollateralAmount = numeric(),
                 CollateralAvgLife = numeric(),
                 BondValue = numeric(),
                 BondValueMethod = "character",
                 BondValueCap = numeric(),
                 BondValueDiscountRate = numeric(),
                 BondValueReinvestmentRate = numeric(),
                 ExpenseBasisPointFee = numeric(),
                 ExpenseFixed = numeric(),
                 ExpensePeriodicity = numeric(),
                 InitialReserveFund = numeric()
  ){
  
  new("RAID",
      DealName = DealName,
      Issuer = Issuer,
      DealNumber = DealNumber,
      DealPriceDate = DealPriceDate,
      DealSettlementDate = DealSettlementDate,
      Underwriter = Underwriter,
      Trustee = Trustee,
      PricingSpeed = PricingSpeed,
      JumpReferenceSpeed1 = JumpReferenceSpeed1,
      JumpReferenceSpeed2 = JumpReferenceSpeed2,
      JumpReferenceSpeed3 = JumpReferenceSpeed3,
      JumpReferenceSpeed4 = JumpReferenceSpeed4,
      NumberofTranches = NumberofTranches,
      NumberofComponentTranches = NumberofComponentTranches,
      NumberofCombinedTranches = NumberofCombinedTranches,
      NumberofPools = NumberofPools,
      PacSchedulesIncluded = PacSchedulesIncluded,
      NumberofPacSchedules = NumberofPacSchedules,
      NumberofGroups = NumberofGroups,      
      DealSize = DealSize,
      CollateralAmount = CollateralAmount,
      CollateralAvgLife = CollateralAvgLife,
      BondValue = BondValue,
      BondValueMethod = BondValueMethod,
      BondValueCap = BondValueCap,
      BondValueDiscountRate = BondValueDiscountRate,
      BondValueReinvestmentRate = BondValueReinvestmentRate,
      ExpenseBasisPointFee = ExpenseBasisPointFee,
      ExpenseFixed = ExpenseFixed,
      ExpensePeriodicity = ExpensePeriodicity,
      InitialReserveFund = InitialReserveFund
  )                 
  }


  #' A constructor function for the REMIC At Issuance Disclosure (RAID) file
  #' 
  #'  The RAID function creates the REMIC At Issuance Disclosure file
  #'  @param DealName A character string the deal name
  #'  @param Issuer A character string the Isser Name
  #'  @param DealNumber A character string the Deal Number
  #'  @param DealPriceDate A character string the Deal Pricing Date
  #'  @param DealSettlementDate A character string the Deal Settlement Date
  #'  @param Underwriter A character string the Deal Underwriter
  #'  @param Trustee A character string the deal Trustee
  #'  @param PricingSpeed A character string the deal pricing speed
  #'  @param JumpReferenceSpeed1 A numeric value the Jump Z speed
  #'  @param JumpReferenceSpeed2 A numeric value the Jump Z speed
  #'  @param JumpReferenceSpeed3 A numeric value the Jump Z speed
  #'  @param JumpReferenceSpeed4 A numeric value the Jump Z speed 
  #'  @param NumberofTranches A numeric value the number of Tranches
  #'  @param NumberofComponentTranches A numeric value the number of component Tranches
  #'  @param NumberofCombinedTranches A numeric value the number of combined Tranches
  #'  @param NumberofPools A numeric value the number of pools
  #'  @param PacSchedulesIncluded A logical value TRUE/FALSE
  #'  @param NumberofPacSchedules A numeric value the number of PAC schedules
  #'  @param NumberofGroups A numeric value the number of groups
  #'  @param DealSize A numeric value the original balance of all tranches
  #'  @param CollateralAmount A numeric value the current face amount of the collateral
  #'  @param CollateralAvgLife A numeric value the avg life of the collateral 
  #'  @param BondValue A numeric value the bond price
  #'  @param BondValueMethod A character string the valuation method
  #'  @param BondValueCap A numeric value the Bond Coupon Cap Rate
  #'  @param BondValueDiscountRate A numeric value the Bond discount rate
  #'  @param BondValueReinvestmentRate A numeric value the assumed trust reinvestment rate
  #'  @param ExpenseBasisPointFee A numeric value the expense of the Trust
  #'  @param ExpenseFixed A numeric value the fixed expenses of the Trust
  #'  @param ExpensePeriodicity A numeric value the calender payment of expenses
  #'  @param InitialReserveFund A numeric value the amount of the initial reserve fund
  #'  @examples
  #'  \dontrun{need example}
  #'@export
  MakeRAID <- function(DealName = "character", 
                     Issuer = "character",
                     DealNumber = "character",
                     DealPriceDate = "character", 
                     DealSettlementDate = "character",
                     Underwriter = "character",
                     Trustee = "character",
                     PricingSpeed = numeric(),
                     JumpReferenceSpeed1 = numeric(),
                     JumpReferenceSpeed2 = numeric(),
                     JumpReferenceSpeed3 = numeric(),
                     JumpReferenceSpeed4 = numeric(),
                     NumberofTranches = numeric(),
                     NumberofComponentTranches = numeric(),
                     NumberofCombinedTranches = numeric(),
                     NumberofPools = numeric(),
                     PacSchedulesIncluded = "logical",
                     NumberofPacSchedules = numeric(),
                     NumberofGroups = numeric(),
                     DealSize = numeric(),
                     CollateralAmount = numeric(),
                     CollateralAvgLife = numeric(),
                     BondValue = numeric(),
                     BondValueMethod = "character",
                     BondValueCap = numeric(),
                     BondValueDiscountRate = numeric(),
                     BondValueReinvestmentRate = numeric(),
                     ExpenseBasisPointFee = numeric(),
                     ExpenseFixed = numeric(),
                     ExpensePeriodicity = numeric(),
                     InitialReserveFund = numeric()){
  
  temp <-RAID(DealName = DealName,
              Issuer = Issuer,
              DealNumber = DealNumber,
              DealPriceDate = DealPriceDate,
              DealSettlementDate = DealSettlementDate,
              Underwriter = Underwriter,
              Trustee = Trustee,
              PricingSpeed = PricingSpeed,
              JumpReferenceSpeed1 = JumpReferenceSpeed1,
              JumpReferenceSpeed2 = JumpReferenceSpeed2,
              JumpReferenceSpeed3 = JumpReferenceSpeed3,
              JumpReferenceSpeed4 = JumpReferenceSpeed4,
              NumberofTranches = NumberofTranches,
              NumberofComponentTranches = NumberofComponentTranches,
              NumberofCombinedTranches = NumberofCombinedTranches,
              NumberofPools = NumberofPools,
              PacSchedulesIncluded = PacSchedulesIncluded,
              NumberofPacSchedules = NumberofPacSchedules,
              NumberofGroups = NumberofGroups,
              DealSize = DealSize,
              CollateralAmount = CollateralAmount,
              CollateralAvgLife = CollateralAvgLife,
              BondValue = BondValue,
              BondValueMethod = BondValueMethod,
              BondValueCap = BondValueCap,
              BondValueDiscountRate = BondValueDiscountRate,
              BondValueReinvestmentRate = BondValueReinvestmentRate,
              ExpenseBasisPointFee = ExpenseBasisPointFee,
              ExpenseFixed = ExpenseFixed,
              ExpensePeriodicity = ExpensePeriodicity,
              InitialReserveFund = InitialReserveFund)
  
  SaveRAID(RAIDFile = temp)}
  
  #=====================================================================================
  #The following are Tranches functions for the REMIC constructor
  #=====================================================================================
  
  
  #1)Tranched Details is a Constructor function to create Tranche data
  # it has no connections and is used by the function MakeTranches

  TrancheDetails <- function(DealName = "character",
                           TrancheNumber = "character",
                           NumberofComponents = numeric(),
                           ComponentofTranches = "character",
                           TrancheName = "character",
                           TranchePrincipal = "character",
                           TranchePrincipalDesc = "character",
                           TrancheInterestDesc = "character",     
                           TrancheOtherDescription = "character",
                           Cusip = "character",
                           TrancheOrigBal = numeric(),
                           TrancheInterest = "character",
                           TrancheCoupon = numeric(),
                           AccrualRate = numeric(),
                           TreasuryMaturity = numeric(),
                           TreasuryYield = numeric(),
                           TreasurySpread = numeric(),
                           TrancheYield = numeric(),
                           TranchePrice = numeric(),
                           TrancheProceedsWithInterest = numeric(),
                           TrancheAvgLife = numeric(),
                           TrancheDuration = numeric(),
                           TrancheDatedDate = numeric(),
                           TrancheFirstPmtDate = "character",
                           TrancheLastPmtDate = "character",
                           TrancheNextPmtDate = "character",
                           TrancheFinalPmtDate = "character",
                           Delay = numeric(),
                           InterestPmtFrequency = numeric(),
                           PrinPmtFrequency = numeric(),
                           PacLowBand = numeric(),
                           PacHighBand = numeric(),
                           FloaterIndex = "character",
                           InitialIndexValue = numeric(),
                           FloaterMargin = numeric(),
                           FloaterMultiplier = numeric(),
                           FloaterCap = numeric(),
                           FloaterFloor = numeric(),
                           FloaterInitialCoupon = numeric(),
                           FloaterResetFrequency = numeric(),
                           FloaterFirstResetDate = "character",
                           FloaterFormula = "function",
                           Group = numeric(),
                           TrancheType = "character",
                           Schedule = "logical",
                           Fixed = "logical"){
  
  new("TrancheDetails",
      DealName = DealName,
      TrancheNumber = TrancheNumber,
      NumberofComponents = NumberofComponents,
      ComponentofTranches = ComponentofTranches,
      TrancheName = TrancheName,
      TranchePrincipal = TranchePrincipal,
      TranchePrincipalDesc = TranchePrincipalDesc,
      TrancheInterestDesc = TrancheInterestDesc,    
      TrancheOtherDescription = TrancheOtherDescription,
      Cusip = Cusip,
      TrancheOrigBal = TrancheOrigBal,
      TrancheInterest = TrancheInterest,
      TrancheCoupon = TrancheCoupon,
      AccrualRate = AccrualRate,
      TreasuryMaturity = TreasuryMaturity,
      TreasuryYield = TreasuryYield,
      TreasurySpread = TreasurySpread,
      TrancheYield = TreasuryYield,
      TranchePrice = TranchePrice,
      TrancheProceedsWithInterest = TrancheProceedsWithInterest,
      TrancheAvgLife = TrancheAvgLife,
      TrancheDuration = TrancheDuration,
      TrancheDatedDate = TrancheDatedDate,
      TrancheFirstPmtDate = TrancheFirstPmtDate,
      TrancheLastPmtDate = TrancheLastPmtDate,
      TrancheNextPmtDate = TrancheNextPmtDate,
      TrancheFinalPmtDate = TrancheFinalPmtDate,
      Delay = Delay,
      InterestPmtFrequency = InterestPmtFrequency,
      PrinPmtFrequency = PrinPmtFrequency,
      PacLowBand = PacLowBand,
      PacHighBand = PacHighBand,
      FloaterIndex = FloaterIndex,
      InitialIndexValue = InitialIndexValue,
      FloaterMargin = FloaterMargin,
      FloaterMultiplier = FloaterMultiplier,
      FloaterCap = FloaterCap,
      FloaterFloor = FloaterFloor,
      FloaterInitialCoupon = FloaterInitialCoupon,
      FloaterResetFrequency = FloaterResetFrequency,
      FloaterFirstResetDate = FloaterFirstResetDate,
      FloaterFormula = FloaterFormula,
      Group = Group,
      TrancheType = TrancheType,
      Schedule = Schedule,
      Fixed = Fixed)
  }


  # 2) Make tranche is actually a function which calls TrancheDetails 
  #serialize the object TrancheDetails to the tranches directory
  #note in the help file MakeTranche is refered to as constructor since
  #the actual constructor TrancheDetails is not exposed to the user

  #' A constructor function for REMIC tranche detail
  #' 
  #' MakeTranche a constructor function used to create a REMIC tranche
  #' @param DealName A character string the deal name
  #' @param TrancheNumber A character string the Tranche Number
  #' @param NumberofComponents A numeric value the number of components
  #' @param ComponentofTranches A string identifying component tranches
  #' @param TrancheName A character string the the Tranche Name
  #' @param TranchePrincipal A character string the principal type (pass-through, notional)
  #' @param TranchePrincipalDesc A character string the REMIC principal type (sequential, IO, PAC, etc.)
  #' @param TrancheInterestDesc A character string the REMIC interest type (Fixed, Floating, Variable)
  #' @param TrancheOtherDescription A character string the other REMIC descriptive information
  #' @param Cusip A character string the tranche cusip
  #' @param TrancheOrigBal A character numeric value the original balance
  #' @param TrancheInterest A character string the Tranche Interest Type
  #' @param TrancheCoupon A numeric value the tranche coupon
  #' @param AccrualRate A numeric value the accrual rate on the bond
  #' @param TreasuryMaturity A numeric value the maturity of the pricing benchmark (Treasury or Swap)
  #' @param TreasuryYield A numeric value the yield of the pricing benchmark (Treasury or Swap)
  #' @param TreasurySpread A numeric value the spread over the pricing bechmark (Treasury or Swap)
  #' @param TrancheYield A numeric value the new issue pricing yield to maturity
  #' @param TranchePrice A numeric value the new issue price
  #' @param TrancheProceedsWithInterest A numeric value the new issue proceeds
  #' @param TrancheAvgLife A numeric value the average life at pricing
  #' @param TrancheDuration A numeric value the duration (modified) at pricing
  #' @param TrancheDatedDate A character value the tranche dated date
  #' @param TrancheFirstPmtDate A character value the tranche first payment date
  #' @param TrancheLastPmtDate A character value the tranche last payment date
  #' @param TrancheNextPmtDate A character value the tranche next payment date
  #' @param TrancheFinalPmtDate A character value the tranche final payment date
  #' @param Delay A numeric value the delay days
  #' @param InterestPmtFrequency A numeric value the interest payment frequency 
  #' @param PrinPmtFrequency A numeric value the principal payment frequency
  #' @param PacLowBand A numeric value the PAC Lower Band
  #' @param PacHighBand A numeric value the PAC Upper Band
  #' @param FloaterIndex A character value the floater index name
  #' @param InitialIndexValue A numeric value the value of the floater's reference index at issuance
  #' @param FloaterMargin A numeric value the floater margin
  #' @param FloaterMultiplier A numeric value the floater multiplier
  #' @param FloaterCap A numeric value the floater cap
  #' @param FloaterFloor A numeric value the floater floor
  #' @param FloaterInitialCoupon A numeric value the floater's new issue coupon
  #' @param FloaterResetFrequency A numeric value the reset frequency (12 is monthly)
  #' @param FloaterFirstResetDate A character string the first reset date following issuance
  #' @param FloaterFormula A function the floater coupon formula
  #' @param Group A numeric value the collateral group number
  #' @param TrancheType A character value the type of tranche (standard, exchangeable)
  #' @param Schedule A logical indicating the PAC/TAC schedule
  #' @param Fixed A logical indicating Fixed (TRUE) or Floating (FALSE) coupon
  #' @examples
  #' \dontrun{Need Example} 
  #'@export
  MakeTranche <- function(  DealName = "character",
                          TrancheNumber = "character",
                          NumberofComponents = numeric(),
                          ComponentofTranches = numeric(),
                          TrancheName = "character",
                          TranchePrincipal = "character",
                          TranchePrincipalDesc = "character",
                          TrancheInterestDesc = "character",     
                          TrancheOtherDescription = "character",
                          Cusip = "character",
                          TrancheOrigBal = numeric(),
                          TrancheInterest = "character",
                          TrancheCoupon = numeric(),
                          AccrualRate = numeric(),
                          TreasuryMaturity = numeric(),
                          TreasuryYield = numeric(),
                          TreasurySpread = numeric(),
                          TrancheYield = numeric(),
                          TranchePrice = numeric(),
                          TrancheProceedsWithInterest = numeric(),
                          TrancheAvgLife = numeric(),
                          TrancheDuration = numeric(),
                          TrancheDatedDate = numeric(),
                          TrancheFirstPmtDate = "character",
                          TrancheLastPmtDate = "character",
                          TrancheNextPmtDate = "character",
                          TrancheFinalPmtDate = "character",
                          Delay = numeric(),
                          InterestPmtFrequency = numeric(),
                          PrinPmtFrequency = numeric(),
                          PacLowBand = numeric(),
                          PacHighBand = numeric(),
                          FloaterIndex = "character",
                          InitialIndexValue = numeric(),
                          FloaterMargin = numeric(),
                          FloaterMultiplier = numeric(),
                          FloaterCap = numeric(),
                          FloaterFloor = numeric(),
                          FloaterInitialCoupon = numeric(),
                          FloaterResetFrequency = numeric(),
                          FloaterFirstResetDate = "character",
                          FloaterFormula = "function",
                          Group = numeric(),
                          TrancheType = "character",
                          Schedule = "logical",
                          Fixed = "logical") {
  
  temp <- TrancheDetails( DealName = DealName,
                          TrancheNumber = TrancheNumber,
                          NumberofComponents = NumberofComponents,
                          ComponentofTranches = ComponentofTranches,
                          TrancheName = TrancheName,
                          TranchePrincipal = TranchePrincipal,
                          TranchePrincipalDesc = TranchePrincipalDesc,
                          TrancheInterestDesc = TrancheInterestDesc,    
                          TrancheOtherDescription = TrancheOtherDescription,
                          Cusip = Cusip,
                          TrancheOrigBal = TrancheOrigBal,
                          TrancheInterest = TrancheInterest,
                          TrancheCoupon = TrancheCoupon,
                          AccrualRate = AccrualRate,
                          TreasuryMaturity = TreasuryMaturity,
                          TreasuryYield = TreasuryYield,
                          TreasurySpread = TreasurySpread,
                          TrancheYield = TreasuryYield,
                          TranchePrice = TranchePrice,
                          TrancheProceedsWithInterest = TrancheProceedsWithInterest,
                          TrancheAvgLife = TrancheAvgLife,
                          TrancheDuration = TrancheDuration,
                          TrancheDatedDate = TrancheDatedDate,
                          TrancheFirstPmtDate = TrancheFirstPmtDate,
                          TrancheLastPmtDate = TrancheLastPmtDate,
                          TrancheNextPmtDate = TrancheNextPmtDate,
                          TrancheFinalPmtDate = TrancheFinalPmtDate,
                          Delay = Delay,
                          InterestPmtFrequency = InterestPmtFrequency,
                          PrinPmtFrequency = PrinPmtFrequency,
                          PacLowBand = PacLowBand,
                          PacHighBand = PacHighBand,
                          FloaterIndex = FloaterIndex,
                          InitialIndexValue = InitialIndexValue,
                          FloaterMargin = FloaterMargin,
                          FloaterMultiplier = FloaterMultiplier,
                          FloaterCap = FloaterCap,
                          FloaterFloor = FloaterFloor,
                          FloaterInitialCoupon = FloaterInitialCoupon,
                          FloaterResetFrequency = FloaterResetFrequency,
                          FloaterFirstResetDate = FloaterFirstResetDate,
                          FloaterFormula = FloaterFormula,
                          Group = Group,
                          TrancheType = TrancheType,
                          Schedule = Schedule,
                          Fixed = Fixed)
  
  SaveTranche(DealName = DealName, TrancheNumber = TrancheNumber, TrancheFile = temp)}


  # 3) tranches assembles the tranches for REMIC structure and is called by REMIC constructor function
  # The function assembles multiple tranches associated with a deal 
  # building the tranche classes into a list
  
  #' Aggregator Function for REMIC constructor
  #' 
  #' Aggregates Tranche data for REMIC constructor
  #' @param NumberofTranches A numeric value the number of tranches in the deal
  #' @param DealName A character string the Deal Name
  #' @export
  Tranches <- function(NumberofTranches = numeric(), DealName = "character"){
    
    TrancheList <- list()
    
    for(i in 1: NumberofTranches){
  
      Tranches <- SaveTranches(DealName = DealName, TrancheNumber = as.character(i))
      
      TrancheList <- append(TrancheList, Tranches)}
    
    new("Tranches",
        Tranches = TrancheList)}

  # --------------------------------------------------------------------------------
  #REMIC Schedules PAC and TAC schedules for REMIC
  #This function is called by MakeSchedules
  #It has no connection strings this functions constructs the PAC REMIC 
  #Class with call to new
  #' @importFrom lubridate %m+%
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
  #' A constructor function to create the PAC Bond Sinking Fund Schedule file
  #' 
  #' Function to create a PAC bond sinking fund schedule file
  #' @param bond.id A character string the cusip or id
  #' @param DealName A character string the transaction deal name
  #' @param Group A character string the tranche's collateral group
  #' @param original.bal A numeric value the collateral group original balance
  #' @param trade.date A character string the trade date
  #' @param settlement.date A character string the settlement date
  #' @param first.pmtdate A character string the bond first payment date
  #' @param price A numeric value the price of the underlying collateral
  #' @param begin.cpr A numeric value the beginning value of the PPC assumption
  #' @param end.cpr A numeric value the ending value of the PPC assumption
  #' @param seasoning.period A numeric value the length of the PPC ramp
  #' @param lower.PSA A numeric value the lower PSA band
  #' @param upper.PSA A numeric value the upper PSA band
  #' @examples 
  #' \dontrun{MakeSchedule(bond.id = "BondLabMBS4",DealName = "BondLabPAC01",Group = 1,
  #' original.bal = 200000000,first.pmtdate = "01-25-2013",trade.date = "01-10-2013",
  #' settlement.date = "01-13-2013",price = 105.75,begin.cpr = .2,end.cpr = 6,
  #' seasoning.period = 30,lower.PSA = 75, upper.PSA = 250)}
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
                         
                         SaveSchedules(DealName = DealName, ScheduleFile = temp)
                         
                         #connSched <- gzfile(description = paste("~/BondLab/Schedules/",
                         #              DealName,"_","Group","_",temp@Group,"_", "Sch", ".rds", sep = ""))
                         #saveRDS(temp, connSched)
}



  # -------- Collateral groups for the REMIC Constructor -------------------------------------------------------------
  # 1) construct the collateral class with call to new.  This function is used by MakeCollateral
 
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
  #' A constructor function to create the collatreal group file for a REMIC
  #' 
  #' Makes Collateral Groups for REMIC structure currently only representative (aggregated collateral groups)
  #' is supported.  In the future multiple collateral pools or loans are envisioned.
  #' @param DealName A character string the deal's name
  #' @param Group A numeric value the collateral group number
  #' @param Cusip A list the collateral group name, collateral pool cusips, or loan numbers.
  #' @param OrigBal A list the original balance of the collateral group name, pool cusip or loan numbers used in the deal
  #' @examples
  #' \dontrun{
  #'  MakeCollateral(DealName = "BondLabPACInverse",
  #'  Group = 1,
  #'  Cusip = list("bondlabMBS4"),
  #'  OrigBal = list("200000000"))
  #' } 
  #'@export
  MakeCollateral <- function(DealName = "character", Group = numeric(), Cusip = list(), OrigBal = list()){
    
    temp <- Collateral(DealName = DealName, Group = Group, Cusip = Cusip, OrigBal = OrigBal)
    
    SaveCollGroup(FileName = temp, DealName = DealName, Group = Group)
    
    #connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group","_",temp@Group,".rds", sep = ""))
    #saveRDS(temp, connGroup)
  }
  
  #------------------------------------------------------------------------------------------
  # 3) aggregator function for the REMIC structure called by REMIC constructor
  # the function assembles multiple collateral groups can be extended to loan level
  
  #' A function to aggregate the collateral group information
  #' 
  #' The function is used by the REMIC Constructor
  #' @param NumberofGroups A numeric value the number collateral groups
  #' @param DealName A character string the Deal Name
  #' @export
  CollateralGroup <- function(NumberofGroups = numeric(), DealName = "character"){
    
    GroupList <- list()
    
    for(i in 1 : NumberofGroups){
      
     connGroup <- REMICGroupConn(DealName = DealName, Group = i)
     #connGroup <- gzfile(description = paste("~/BondLab/Groups/",DealName,"_","Group","_",i,".rds", sep = "")) 
     
     Group <- readRDS(connGroup)
     GroupList <- append(GroupList, Group)
    }
    new("CollateralGroup",
        Group = GroupList)
    
  }
  # ------ RDME Functions for the REMIC structuring tool ---------------------------------------------------------
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
    

  
  # 2) serailize tranche factor date to RDME directory
  #' A constructor function for the REMIC Disclosure Month End (RDME) file
  #' 
  #' A constructor for the REMIC Month End Discloure.  This file the monthly factor data for each Tranche
  #' @param DealName A charcter string the deal name
  #' @param TrancheNumber A numeric value the number of the Tranche
  #' @param Cusip A character string the tranche cusip
  #' @param PaymentDate A character string the payment date coinciding with the factor data
  #' @param Coupon A numeric value the tranche's coupon
  #' @param Factor A numeric value the tranche's factor
  #' @examples
  #' \dontrun{
  #'MakeRDME(DealName = "BondLabPACInverse",
  #'TrancheNumber = 1,
  #'Cusip = "BondLabPAC2",
  #'"PaymentDate = "01-01-2013",
  #'Coupon = 2.25,
  #'Factor = 1)
  #'
  #'MakeRDME(DealName = "BondLabPACInverse",
  #'       TrancheNumber = 2,
  #'       Cusip = "BondLabFltr",
  # '      PaymentDate = "1-01-2013",
  #'       Coupon = 0.55,
  #'       Factor = 1)
  #'
  #'MakeRDME(DealName = "BondLabPACInverse",
  #'       TrancheNumber = 3,
  #'       Cusip = "BondLabCMP1",
  #'       PaymentDate = "1-01-2013",
  #'       Coupon = 9.21,
  #'       Factor = 1)
  #'
  #'MakeRDME(DealName = "BondLabPACInverse",
  #'       TrancheNumber = 4,
  #'       Cusip = "BondLabPACIO",
  #'       PaymentDate = "1-01-2013",
  #'       Coupon = 1.75,
  #'       Factor = 1)
  #' } 
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
    
    
    SaveRDME(FileName = temp, DealName = DealName, TrancheNumber = TrancheNumber)
    #connRDME <- gzfile(description = paste("~/BondLab/RDME/",DealName,"_","Tranche","_",TrancheNumber,"_","Factor",".rds", sep = ""))
    #saveRDS(temp, connRDME)
    #close(connRDME)
    
  }
  

    # 3) aggregator function for tranche factor information called by REMIC contructor
    #' A function to aggregate monthly updated factor data
    #' 
    #' Aggregator function used by the REMIC constructor to aggregate Monthly Factor Data
    #' @param NumberofTranches A numeric value the Number of traches related to the collateral group
    #' @param DealName A character string the Deal Name
    #' @export
    RDMEData <- function(NumberofTranches = numeric(), DealName = "character"){
    RDMEList <- list()
    
    for(i in 1 : NumberofTranches){
      
      
      RDMEFactor <- RDMEFactor(DealName = DealName, TrancheNumber = i)
      #TrancesFactor <- function(DealName = "character", TrancheNumber = numeric()){
      #connRDME <- gzfile(description = paste("~/BondLab/RDME/",DealName,"_","Tranche","_",TrancheNumber,"_","Factor",".rds", sep = "")) 
      #RDMEFactor <- readRDS(connRDME)
      #return(RDMEFactor)
      #close(connRDME)
      #}
      
      RDMEList <- append(RDMEList, RDMEFactor)
    }
    new("TrancheFactors",
        FactorData = RDMEList)
    
  }
  
  # --- REMIC Constructor Function these functions are used to assemble the 
  # --- RAID, Tranches, RDME, and Collateral Groups into a single REMIC structure

  #' The constructor function to build the REMIC deal from each of its elements 
  #' 
  #' The function Assembles the deal RAID, RDME, Tranches, and Groups files into a REMIC structure
  #' @param DealName A character string the deal's names
  #' @examples
  #' \dontrun{
  #' RemicStructure("BondLabPACInverse")  
  #' }
  #'@export
  RemicStructure <- function(DealName = "character"){
    
    RAID <- ReadRAID(RAIDFile = DealName)
    Tranche <- Tranches(NumberofTranches = RAID@NumberofTranches, 
                        DealName = RAID@DealName)
    
    CollateralGroupData <- CollateralGroup(NumberofGroups = RAID@NumberofGroups, 
                                           DealName = RAID@DealName)
    FactorData <- RDMEData(NumberofTranches = RAID@NumberofTranches, 
                           DealName = RAID@DealName)
    
    REMIC <-new("REMICStructure", 
          
          DealName = RAID@DealName,
          Issuer = RAID@Issuer,
          DealNumber = RAID@DealNumber,
          DealPriceDate = RAID@DealPriceDate,
          DealSettlementDate = RAID@DealSettlementDate,
          Underwriter = RAID@Underwriter,
          Trustee = RAID@Trustee,
          PricingSpeed = RAID@PricingSpeed,
          JumpReferenceSpeed1 = RAID@JumpReferenceSpeed1,
          JumpReferenceSpeed2 = RAID@JumpReferenceSpeed2,
          JumpReferenceSpeed3 = RAID@JumpReferenceSpeed3,
          JumpReferenceSpeed4 = RAID@JumpReferenceSpeed4,
          NumberofTranches = RAID@NumberofTranches,
          NumberofComponentTranches = RAID@NumberofComponentTranches,
          NumberofCombinedTranches = RAID@NumberofCombinedTranches,
          NumberofPools = RAID@NumberofPools,
          PacSchedulesIncluded = RAID@PacSchedulesIncluded,
          NumberofPacSchedules = RAID@NumberofPacSchedules,
          NumberofGroups = RAID@NumberofGroups,
          DealSize = RAID@DealSize,
          CollateralAmount = RAID@CollateralAmount,          
          BondValue = RAID@BondValue,
          BondValueMethod = RAID@BondValueMethod,
          BondValueCap = RAID@BondValueCap,
          BondValueDiscountRate = RAID@BondValueDiscountRate,
          BondValueReinvestmentRate = RAID@BondValueReinvestmentRate,
          ExpenseBasisPointFee = RAID@ExpenseBasisPointFee,
          ExpenseFixed = RAID@ExpenseFixed,
          ExpensePeriodicity = RAID@ExpensePeriodicity,
          InitialReserveFund = RAID@InitialReserveFund,
          Tranches = Tranche@Tranches,
          CollateralGroup = CollateralGroupData@Group,
          TrancheFactors = FactorData@FactorData)
    
          SaveREMIC(DealName = DealName, file = REMIC)

  }
  

