# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 


# This section begins REMIC Methods the methods are used for the analysis of REMIC
# REMIC functions require the superclass REMIC structure and the call to source(waterfall file)

# ======= REMIC constructor methods create null empty classes to be populated by "new" constructors

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


