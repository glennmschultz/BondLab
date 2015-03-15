# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#======================  Set and create class Bond Details ==============================
# Do I need a generic for this?
# ---------------------       Inititialize BoneDetails     ------------------------------

  setMethod("initialize",
          signature = ("BondDetails"),
          function(.Object,
                   Cusip = "character",
                   ID ="character",
                   BondType = "character",
                   Sector ="character",
                   Coupon = "numeric",
                   IssueDate = "character",
                   DatedDate = "character",
                   Maturity = "character",
                   LastPmtDate = "character",
                   NextPmtDate = "character",
                   Moody = "character",
                   SP = "character",
                   BondLab  = "character",
                   Frequency = "numeric",
                   BondBasis = "character",
                   Callable = "character",
                   Putable = "character",
                   SinkingFund = "character")
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
              .Object@Moody
              .Object@SP
              .Object@BondLab
              .Object@Frequency
              .Object@BondBasis
              .Object@Callable
              .Object@Putable
              .Object@SinkingFund
              
              return(.Object)
              callNextMethod(.Object,...)
          })

  BondDetails <-function(
    Cusip = "character",
    ID ="character",
    BondType = "character",
    Sector ="character",
    Coupon = numeric(),
    IssueDate = "character",
    DatedDate = "character",
    Maturity = "character",
    LastPmtDate = "character",
    NextPmtDate = "character",
    Moody = "character",
    SP = "character",
    BondLab  = "character",
    Frequency = numeric(),
    BondBasis = "character",
    Callable = "character",
    Putable = "character",
    SinkingFund = "character") {
    
    new("BondDetails",
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
        Moody = Moody,
        SP = SP,
        BondLab = BondLab,
        Frequency = Frequency,
        BondBasis = BondBasis,
        Callable = Callable,
        Putable = Putable,
        SinkingFund = SinkingFund)
  }

#' @export
  MakeBondDetails <- function(    
    Cusip = "character",                              
    ID ="character",                              
    BondType = "character",                              
    Sector ="character",
    Coupon = numeric(),
    IssueDate = "character",
    DatedDate = "character",
    Maturity = "character",
    LastPmtDate = "character",
    NextPmtDate = "character",
    Moody = "character",
    SP = "character",
    BondLab  = "character",
    Frequency = numeric(),
    BondBasis = "character",
    Callable = "character",
    Putable = "character",
    SinkingFund = "character")
  {
    temp <- BondDetails(
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
     Moody = Moody,
     SP = SP,
     BondLab = BondLab,
     Frequency = Frequency,
     BondBasis = BondBasis,
     Callable = Callable,
     Putable = Putable,
     SinkingFund = SinkingFund
      )
    
    connBondDetails <- gzfile(description = paste("~/BondLab/BondData/",temp@ID,".rds", sep = ""))
    saveRDS(temp, connBondDetails)
    close(connBondDetails)
  }

  setGeneric("MakeBondDetails", function(
    Cusip = "character",                              
    ID ="character",                              
    BondType = "character",                              
    Sector ="character",
    Coupon = numeric(),
    IssueDate = "character",
    DatedDate = "character",
    Maturity = "character",
    LastPmtDate = "character",
    NextPmtDate = "character",
    Moody = "character",
    SP = "character",
    BondLab  = "character",
    Frequency = numeric(),
    BondBasis = "character",
    Callable = "character",
    Putable = "character",
    SinkingFund = "character")
  {standardGeneric("MakeBondDetails")})
        