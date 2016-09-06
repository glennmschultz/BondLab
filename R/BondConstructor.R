

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.

  #' @include PassThroughConstructor.R
  NULL
 
  #' An S4 class representing the cusip detail of the a standard bond
  #' 
  #' @slot Cusip a character string of length 9 the bond's cusip
  #' @slot ID a character string the ID of the bond
  #' @slot BondType a character string the type of the bond UST, Corp
  #' @slot Sector a character string the bond sector 
  #' (Govt, Financial, Utility, etc.)
  #' @slot Coupon a numeric value the bond's coupon
  #' @slot IssueDate a character string the issue date in mm/dd/YYYY format
  #' @slot DatedDate a character string the dated date in mm/dd/YYYY format
  #' @slot Maturity a character string the maturity date in mm/dd/YYYY format
  #' @slot LastPmtDate a character string the payment date the date of the last
  #' payment received by the investor in mm/dd/YYYY format
  #' @slot NextPmtDate a character string the payment date of the next scheduled
  #' payment to be received by the investor
  #' @slot Moody A character string the Moody credit rating
  #' @slot SP A character string the S&P credit rating
  #' @slot BondLab A character string the BondLab or investor 
  #' assigned credit rating
  #' @slot Frequency A numeric value the payment frequency of the bond 
  #' (the number of payments made in one-year)
  #' @slot BondBasis a character string the bond's interest calculation basis
  #' @slot Callable a logical indicating the bond is callable
  #' @slot Putable a logical indicating the bond is putable
  #' @slot SinkingFund a logical indicating the bond has a sinking fund schedule
  #' @exportClass BondDetails 
  setClass("BondDetails",
         representation(
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
           Moody = "character",
           SP = "character",
           BondLab  = "character",
           Frequency = "numeric",
           BondBasis = "character",
           Callable = "logical",
           Putable = "logical",
           SinkingFund = "logical"))
  
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
    Callable = "logical",
    Putable = "logical",
    SinkingFund = "logical")
  {standardGeneric("MakeBondDetails")})
  
  # standard generic cusip is defined in the passthrough constructor
  # standard generic ID is defined in the passthrough constructor
  # standard generic BondType is defined in the passthrough constructor
  # standard generic Sector is defined in the passthrough constructor
  # standard generic Coupon is defined in the passthrough constructor
  # standard generic IssueDate is defined in the passthrough constructor
  # standard generic DatedDate is defined in the passthrough constructor
  # standard generic Maturity is defined in the passthrogh constructor
  # standard generic LastPmtDate is defined in the passthrough constructor
  # standard generic NextPmtDate is defined in the passthrough constructor
  # standard generic NextPmtdate <- is defined in the passthrough constructor
  # standard generic MoodyRating is defined in the passthrough constructor
  # standard generic SPRating is defined in passthrough constructor
  # standard generic BondLabRating is defined in passthough constructor
  # standard generic Frequency is defined in passthrough constructor
  # standard generic BondBasis is defined in passthrough constructor

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
                   Callable = "logical",
                   Putable = "logical",
                   SinkingFund = "logical",
                   ...){
            callNextMethod(.Object,
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
                           SinkingFund = SinkingFund,
                           ...)
          })
  
  #' A method to extract Cusip from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod Cusip
  setMethod("Cusip", signature("BondDetails"),
            function(object){object@BondDetails})
  
  #' A method to extract ID from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object BondDetails
  #' @exportMethod ID
  setMethod("ID", signature("BondDetails"),
            function(object){object@BondDetails})
  
  #' A method to extract BondType from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod BondType
  setMethod("BondType", signature("BondDetails"),
            function(object){object@BondDetails})
  
  #' A method to extract Sector from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod Sector
  setMethod("Sector", signature("BondDetails"),
            function(object){object@BondDetails})
  
  #' A method to extract Coupon from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 object of type BondDetails
  #' @exportMethod Cusip
  setMethod("Cusip", signature("BondDetails"),
            function(object){object@BondDetails})
  
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
    Callable = "logical",
    Putable = "logical",
    SinkingFund = "logical") {
    
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
  #' @param Coupon A character string the coupon (interest rate) 
  #' paid to the investor
  #' @param IssueDate A character string the issue date of the bond
  #' @param DatedDate A character string the bond's dated date
  #' @param Maturity A character string the bond's maturity date of final 
  #' principal payment date
  #' @param LastPmtDate A character string the most recent payment 
  #' date to the investor
  #' @param NextPmtDate A character string the next payment date due to 
  #' the investor
  #' @param Moody A character string the Moody assigned rating
  #' @param SP A character string the SP assigned rating
  #' @param BondLab A character string the Bond Lab assigned rating
  #' @param Frequency A numeric value the payment frequency
  #' @param BondBasis A character string the interest calculation basis
  #' @param Callable A character string the bond's call schedule
  #' @param Putable A caracter string the bond's put schedule
  #' @param SinkingFund a character strign the bond sinking fund schedule
  #' @export MakeBondDetails
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
    SaveBond(filename = temp)

#connBondDetails <- gzfile(description = paste("~/BondLab/BondData/",temp@ID,".rds", sep = ""))
#saveRDS(temp, connBondDetails)
#close(connBondDetails)
  }


        