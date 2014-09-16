# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#======================  Set and create class MBS Details ==============================
#setGeneric("MBSDetails", function(Cusip = "character", ID = "character", BondType = "character", Sector ="character", Coupon = numeric(),
#                                  IssueDate = "character", DatedDate = "character", Maturity = "character", LastPmtDate = "character", NextPmtDate = "character",
#                                  PaymentDelay = numeric(), Moody = "character", SP = "character", BondLab  = "character", Frequency = numeric(), 
#                                  BondBasis = "character", GWac = numeric(), AmortizationType = "character", AmortizationTerm = numeric(), Index = "character",
#                                  Margin = numeric(), FirstPmtDate = "character", FinalPmtDate = "character", Servicing = numeric(), PMI = numeric(),
#                                  Gfee = numeric(), InitialInterest = "character", InterestOnlyPeriod = numeric(), FirstPrinPaymentDate = "character",
#                                  BalloonPmt = "character", BalloonDate = "character", MBSFactor = numeric(), Model = "character", Burnout = numeric(), SATO = numeric()) 
#                                  {standardGeneric("MBSDetails")})



# Inititialize MBSDetails
setMethod("initialize",
          signature("MBSDetails"),
          function(.Object,
                   Cusip = "character",
                   ID = "character",
                   BondType = "character",
                   Sector ="character",
                   Coupon = "numeric",
                   IssueDate = "character",
                   DatedDate = "character",
                   Maturity = "character",
                   LastPmtDate = "character",
                   NextPmtDate = "character",
                   PaymentDelay = "numeric",
                   Moody = "character",
                   SP = "character",
                   BondLab  = "character",
                   Frequency = "numeric",
                   BondBasis = "character",
                   GWac = "numeric",
                   AmortizationType = "character",
                   AmortizationTerm = "numeric",
                   Index = "character",
                   Margin = "numeric",
                   FirstPmtDate = "character",
                   FinalPmtDate = "character",
                   Servicing = "numeric",
                   PMI = "numeric",
                   Gfee = "numeric",
                   InitialInterest = "character",
                   InterestOnlyPeriod = "numeric",
                   FirstPrinPaymentDate = "character",
                   BalloonPmt = "character",
                   BalloonDate = "character",
                   MBSFactor = "numeric",
                   Model = "character",
                   Burnout = "numeric",
                   SATO = "numeric")
          
{
            .Object@Cusip = Cusip
            .Object@ID = ID
            .Object@BondType = BondType
            .Object@Sector = Sector
            .Object@Coupon = Coupon
            .Object@IssueDate = IssueDate
            .Object@DatedDate = DatedDate
            .Object@Maturity = Maturity
            .Object@LastPmtDate = LastPmtDate
            .Object@NextPmtDate = NextPmtDate
            .Object@PaymentDelay = PaymentDelay
            .Object@Moody = Moody
            .Object@SP = SP
            .Object@BondLab = BondLab
            .Object@Frequency = Frequency
            .Object@BondBasis = BondBasis
            .Object@GWac = GWac
            .Object@AmortizationType = AmortizationType
            .Object@AmortizationTerm = AmortizationTerm
            .Object@Index = Index
            .Object@Margin = Margin
            .Object@FirstPmtDate = FirstPmtDate
            .Object@FinalPmtDate = FinalPmtDate
            .Object@Servicing = Servicing
            .Object@PMI = PMI
            .Object@Gfee = Gfee
            .Object@InitialInterest = InitialInterest
            .Object@InterestOnlyPeriod = InterestOnlyPeriod
            .Object@FirstPrinPaymentDate = FirstPrinPaymentDate
            .Object@BalloonPmt = BalloonPmt
            .Object@BalloonDate = BalloonDate
            .Object@MBSFactor = MBSFactor
            .Object@Model = Model
            .Object@Burnout = Burnout
            .Object@SATO = SATO
            
            return(.Object)
            callNextMethod(.Object,...)
          })



  MBSDetails <- function(
  Cusip = "character",
  ID = "character",
  BondType = "character",
  Sector ="character",
  Coupon = numeric(),
  IssueDate = "character",
  DatedDate = "character",
  Maturity = "character",
  LastPmtDate = "character",
  NextPmtDate = "character",
  PaymentDelay = numeric(),
  Moody = "character",
  SP = "character",
  BondLab  = "character",
  Frequency = numeric(),
  BondBasis = "character",
  GWac = numeric(),
  AmortizationType = "character",
  AmortizationTerm = numeric(),
  Index = "character",
  Margin = numeric(),
  FirstPmtDate = "character",
  FinalPmtDate = "character",
  Servicing = numeric(),
  PMI = numeric(),
  Gfee = numeric(),
  InitialInterest = "character",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "character",
  BalloonDate = "character",
  MBSFactor = numeric(),
  Model = "character",
  Burnout = numeric(),
  SATO = numeric()
  ) {
  
  new("MBSDetails",
      Cusip = Cusip,
      ID = ID,
      BondType = BondType,
      Sector = Sector,
      Coupon = Coupon,
      IssueDate = IssueDate,
      DatedDate = DatedDate,
      Maturity = Maturity,
      LastPmtDate = LastPmtDate,
      NextPmtDate = NextPmtDate,
      PaymentDelay = PaymentDelay,
      Moody = Moody,
      SP = SP,
      BondLab  = BondLab,
      Frequency = Frequency,
      BondBasis = BondBasis,
      GWac = GWac,
      AmortizationType = AmortizationType,
      AmortizationTerm = AmortizationTerm,
      Index = Index,
      Margin = Margin,
      FirstPmtDate = FirstPmtDate,
      FinalPmtDate = FinalPmtDate,
      Servicing = Servicing,
      PMI = PMI,
      Gfee = Gfee,
      InitialInterest = InitialInterest,
      InterestOnlyPeriod = InterestOnlyPeriod,
      FirstPrinPaymentDate = FirstPrinPaymentDate,
      BalloonPmt = BalloonPmt,
      BalloonDate = BalloonDate,
      MBSFactor = MBSFactor,
      Model = Model,
      Burnout = Burnout,
      SATO = SATO)
  }



# ============= This function constructs and saves the MBSDetails files ====================  
  
  MakeMBSDetails <- function(
  Cusip = "character",
  ID = "character",
  BondType = "character",
  Sector ="character",
  Coupon = numeric(),
  IssueDate = "character",
  DatedDate = "character",
  Maturity = "character",
  LastPmtDate = "character",
  NextPmtDate = "character",
  PaymentDelay = numeric(),
  Moody = "character",
  SP = "character",
  BondLab  = "character",
  Frequency = numeric(),
  BondBasis = "character",
  GWac = numeric(),
  AmortizationType = "character",
  AmortizationTerm = numeric(),
  Index = "character",
  Margin = numeric(),
  FirstPmtDate = "character",
  FinalPmtDate = "character",
  Servicing = numeric(),
  PMI = numeric(),
  Gfee = numeric(),
  InitialInterest = "character",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "character",
  BalloonDate = "character",
  MBSFactor = numeric(),
  Model = "character",
  Burnout = numeric(),
  SATO = numeric())
{
  
  temp <- MBSDetails(
    Cusip = Cusip,
    ID = ID,
    BondType = BondType,
    Sector = Sector,
    Coupon = Coupon,
    IssueDate = IssueDate,
    DatedDate = DatedDate,
    Maturity = Maturity,
    LastPmtDate = LastPmtDate,
    NextPmtDate = NextPmtDate,
    PaymentDelay = PaymentDelay,
    Moody = Moody,
    SP = SP,
    BondLab  = BondLab,
    Frequency = Frequency,
    BondBasis = BondBasis,
    GWac = GWac,
    AmortizationType = AmortizationType,
    AmortizationTerm = AmortizationTerm,
    Index = Index,
    Margin = Margin,
    FirstPmtDate = FirstPmtDate,
    FinalPmtDate = FinalPmtDate,
    Servicing = Servicing,
    PMI = PMI,
    Gfee = Gfee,
    InitialInterest = InitialInterest,
    InterestOnlyPeriod = InterestOnlyPeriod,
    FirstPrinPaymentDate = FirstPrinPaymentDate,
    BalloonPmt = BalloonPmt,
    BalloonDate = BalloonDate,
    MBSFactor = MBSFactor,
    Model = Model,
    Burnout = Burnout,
    SATO = SATO)
  
  connMBSDetails <- gzfile(description = paste("~/BondLab/BondData/",temp@ID,".rds", sep = ""))
  saveRDS(temp, connMBSDetails)
  close(connMBSDetails)
}

