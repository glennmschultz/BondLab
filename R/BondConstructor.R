  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Bond Lab Technologies, Inc.
  # Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
  # book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

  #======================  Set and create class Bond Details ==============================

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
              .Object@Moody = Moody
              .Object@SP = SP
              .Object@BondLab = BondLab
              .Object@Frequency = Frequency
              .Object@BondBasis = BondBasis
              .Object@Callable = Callable
              .Object@Putable = Putable
              .Object@SinkingFund = SinkingFund
              
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
#' A constuctor function to create standard bond cusip information
#' 
#' This is a standard generic function used to construct a tradtional bond
#' @param Cusip A character string the bond's cusip number
#' @param ID A character string the bond's ID
#' @param BondType A character string the type of bond
#' @param Sector A character string the bond sector
#' @param Coupon A character string the coupon (interest rate) paid to the investor
#' @param IssueDate A character string the issue date of the bond
#' @param DatedDate A character string the bond's dated date
#' @param Maturity A character string the bond's maturity date of final principal payment date
#' @param LastPmtDate A character string the most recent payment date to the investor
#' @param NextPmtDate A character string the next payment date due to the investor
#' @param Moody A character string the Moody assigned rating
#' @param SP A character string the SP assigned rating
#' @param BondLab A character string the Bond Lab assigned rating
#' @param Frequency A numeric value the payment frequency
#' @param BondBasis A character string the interest calculation basis
#' @param Callable A character string the bond's call schedule
#' @param Putable A caracter string the bond's put schedule
#' @param SinkingFund a character strign the bond sinking fund schedule
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
        