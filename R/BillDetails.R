  
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.


  #' @include MBSDetails.R BondDetails.R
  NULL
  
  #' An S4 class representing the cusip detail of a discount bill
  #' 
  #' @slot Cusip a character string of length 9 the bond's cusip
  #' @slot ID a character string the ID of the bond
  #' @slot BillType a character string the type of the bond UST, Corp
  #' @slot Sector a character string the bond sector 
  #' (Govt, Financial, Utility, etc.)
  #' @slot Issuer a character string the issuer of the bill, note, or bond.
  #' @slot Underwriter a character string the deal underwriter
  #' @slot OfferAmount a numeric value the OfferAmount Issued.
  #' @slot IssueDiscountRate a numeric value the bond's coupon
  #' @slot IssueDate A character the issue date mm-dd-YYYY
  #' @slot DatedDate A character the dated date mm-dd-YYYY
  #' @slot Maturity a character string the maturity date in mm/dd/YYYY format
  #' @slot Moody A character string the Moody credit rating
  #' @slot SP A character string the S&P credit rating
  #' @slot BondLab A character string the BondLab or investor 
  #' assigned credit rating
  #' @slot BondBasis a character string the bond's interest calculation basis
  #' @exportClass BillDetails 
  setClass("BillDetails",
           representation(
             Cusip = "character",
             ID = "character",
             BillType = "character",
             Sector ="character",
             Issuer = "character",
             Underwriter = "character",
             OfferAmount = "numeric",
             IssueDiscountRate = "numeric",
             IssueDate = "character",
             DatedDate = "character",
             Maturity = "character",
             Moody = "character",
             SP = "character",
             BondLab  = "character",
             BondBasis = "character"))
  
  # standard generic cusip is defined in the MBSDetails
  # standard generic ID is defined in the MBSDetails
  # standard generic BondType is defined in the MBSDetails
  # standard generic Sector is defined in the MBSDetails
  # standard generic Issuer is defined in the MBSDetails
  # standard generic underwriter is defined in MBSDetails
  
  #' A standard generic function to get the BillType
  #' 
  #' @param object An S4 class object
  #' @export BillType
  setGeneric("BillType", function(object)
    {standardGeneric("BillType")})
  
  #' A standard generic function to get the IssueDiscount Rate
  #' 
  #' @param object An S4 class object
  #' @export IssueDiscountRate
  setGeneric("IssueDiscountRate", function(object)
    {standardGeneric("IssueDiscountRate")})
  
  # standard generic OfferAmount is defined in BondDetails
  # standard generic IssueDate is defined in the MBSDetails
  # standard generic DatedDate is defined in the MBSDetails
  # standard generic Maturity is defined in the MBSDetails
  # standard generic MoodyRating is defined in the MBSDetails
  # standard generic SPRating is defined in MBSDetails
  # standard generic BondLabRating is defined in MBSDetails
  # standard generic BondBasis is defined in MBSDetails
  
  setMethod("initialize",
            signature("BillDetails"),
            function(.Object,
                     Cusip = "character",
                     ID ="character",
                     BillType = "character",
                     Sector ="character",
                     Issuer = "character",
                     Underwriter = "character",
                     OfferAmount = "numeric",
                     IssueDiscountRate = "numeric",
                     IssueDate = "character",
                     DatedDate = "character",
                     Maturity = "character",
                     Moody = "character",
                     SP = "character",
                     BondLab  = "character",
                     BondBasis = "character",
                     ...){
              callNextMethod(.Object,
                             Cusip = Cusip,
                             ID = ID,
                             BillType = BillType,
                             Sector = Sector,
                             Issuer = Issuer,
                             Underwriter = Underwriter,
                             OfferAmount = OfferAmount,
                             IssueDiscountRate = IssueDiscountRate,
                             IssueDate = IssueDate,
                             DatedDate = DatedDate,
                             Maturity = Maturity,
                             Moody = Moody,
                             SP = SP,
                             BondLab = BondLab,
                             BondBasis = BondBasis,
                             ...)
            })
  
  #' Method to extract Cusip from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 object of type BillDetails
  #' @exportMethod Cusip
  setMethod("Cusip", signature("BillDetails"),
            function(object){object@Cusip})
  
  #' Method to extract ID from an S4 class of type BillDetail
  #' 
  #' @param object The name of the S4 object of type BillDetails
  #' @exportMethod ID
  setMethod("ID", signature("BillDetails"),
            function(object){object@ID})
  
  #' Method to extract BillType from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 object of type BillDetails
  #' @exportMethod BillType
  setMethod("BillType", signature("BillDetails"),
            function(object){object@BillType})
  
  #' Method to extract BillType from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod Sector
  setMethod("Sector", signature("BillDetails"),
            function(object){object@Sector})
  
  #' Method to extract BillType from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BIllDetails
  #' @exportMethod Issuer
  setMethod("Issuer", signature("BillDetails"),
            function(object){object@Issuer})
  
  #' Method to extract BillType from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod Underwriter 
  setMethod("Underwriter", signature("BillDetails"),
            function(object){object@Underwriter})
  
  #' Method to extract OfferAmount from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod OfferAmount
  setMethod("OfferAmount", signature("BillDetails"),
            function(object){object@OfferAmount})
  
  #' Method to extract IssueDiscountRate from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod IssueDiscountRate
  setMethod("IssueDiscountRate", signature("BillDetails"),
            function(object){object@IssueDiscountRate})
  
  #' Method to extract IssueDate from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod IssueDate
  setMethod("IssueDate", signature("BillDetails"),
            function(object){object@IssueDate})
  
  #' Method to extract DatedDate from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod DatedDate
  setMethod("DatedDate", signature("BillDetails"),
            function(object){object@DatedDate})
  
  #' Method to extract MaturityDate from an S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod Maturity
  setMethod("Maturity", signature("BillDetails"),
            function(object){object@Maturity})
  
  #' Method to extract Moody rating from S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod MoodyRating
  setMethod("MoodyRating", signature("BillDetails"),
            function(object){object@Moody})
  
  #' Method to extract S&P rating from S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod SPRating
  setMethod("SPRating", signature("BillDetails"),
            function(object){object@SP})
  
  #' Method to extract BondLab rating from S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod BondLabRating
  setMethod("BondLabRating", signature("BillDetails"),
            function(object){object@BondLab})
  
  #' Method to extract BondBasis of the S4 class of type BillDetails
  #' 
  #' @param object The name of the S4 class of type BillDetails
  #' @exportMethod BondBasis
  setMethod("BondBasis", signature("BillDetails"),
            function(object){object@BondBasis})
  
  #' @title BillDetails
  #' @family BillDetails
  #' @description BillDetails creates BillDetails cusip object in the 
  #' local environment
  #' @param Cusip A character the bill cusip
  #' @param ID A character the bill id
  #' @param BillType A character the type of bill
  #' @param Sector A character the sector 
  #' @param Issuer A character the Issuer
  #' @param Underwriter A character the Underwriter
  #' @param OfferAmount A numeric value the offer amount
  #' @param IssueDiscountRate A numeric value the discount rate
  #' @param IssueDate A character mm-dd-YYYY
  #' @param DatedDate A character mm-dd-YYYY
  #' @param Maturity A character mm-dd-YYYY
  #' @param Moody A character the Moody Rating
  #' @param SP A character the SP Rating
  #' @param BondLab A character the BondLab (Investor) assigned rating
  #' @param BondBasis A character the interest calculation basis
  #' @export BillDetails
  BillDetails <- function(
    Cusip,
    ID,
    BillType,
    Sector,
    Issuer,
    Underwriter,
    OfferAmount,
    IssueDiscountRate,
    IssueDate,
    DatedDate,
    Maturity,
    Moody,
    SP,
    BondLab,
    BondBasis){
    new("BillDetails",
        Cusip = Cusip,
        ID = ID,
        BillType = BillType,
        Sector = Sector,
        Issuer = Issuer,
        Underwriter = Underwriter,
        OfferAmount = OfferAmount,
        IssueDiscountRate = IssueDiscountRate,
        IssueDate = IssueDate,
        DatedDate = DatedDate,
        Maturity = Maturity,
        Moody = Moody,
        SP = SP,
        BondLab = BondLab,
        BondBasis = BondBasis)
  }
  
  # Note there is no MakeBillDetails function