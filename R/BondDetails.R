

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2016  Bond Lab Technologies, Inc.
  # 
  # This program is free software: you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation, either version 3 of the License, or
  # (at your option) any later version.
  # 
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with this program.  If not, see <http://www.gnu.org/licenses/>.


  #' @include MBSDetails.R 
  NULL
 
  #' An S4 class representing the cusip detail of the a standard bond
  #' 
  #' @slot Cusip a character string of length 9 the bond's cusip
  #' @slot ID a character string the ID of the bond
  #' @slot BondType a character string the type of the bond UST, Corp
  #' @slot Sector a character string the bond sector 
  #' (Govt, Financial, Utility, etc.)
  #' @slot Issuer a character string the issuer of the bill, note, or bond.
  #' @slot Underwriter a character string the deal underwriter
  #' @slot OfferAmount a numeric value the OfferAmount Issued.
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
           Issuer = "character",
           Underwriter = "character",
           OfferAmount = "numeric",
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

  
  # standard generic cusip is defined in the MBSDetails
  # standard generic ID is defined in the MBSDetails
  # standard generic BondType is defined in the MBSDetails
  # standard generic Sector is defined in the MBSDetails
  # standard generic Issuer is defined in the MBSDetails
  # standard generic underwriter is defined in MBSDetails
  
  #' A standard generic function to get the par amount isssued traded
  #' 
  #' @param object an S4 class object
  #' @export OfferAmount
  setGeneric("OfferAmount", function(object)
    {standardGeneric("OfferAmount")})
  
  # standard generic Coupon is defined in the MBSDetails
  # standard generic IssueDate is defined in the MBSDetails
  # standard generic DatedDate is defined in the MBSDetails
  # standard generic Maturity is defined in the MBSDetails
  # standard generic LastPmtDate is defined in the MBSDetails
  # standard generic NextPmtDate is defined in the MBSDetails
  # standard generic NextPmtdate <- is defined in the MBSDetails
  # standard generic MoodyRating is defined in the MBSDetails
  # standard generic SPRating is defined in MBSDetails
  # standard generic BondLabRating is defined in MBSDetails
  # standard generic Frequency is defined in MBSDetails
  # standard generic BondBasis is defined in MBSDetails
  
  #' A standard generic function to get the slot Callable
  #' 
  #' @param object an S4 class object
  #' @export Callable
  setGeneric("Callable", function(object)
    {standardGeneric("Callable")})
  
  #' A standard generic function to get the slot Putable
  #' 
  #' @param object an S4 class object
  #' @export Putable
  setGeneric("Putable", function(object)
    {standardGeneric("Putable")})
  
  #' A standard generic function to get the slot SinkingFund
  #' 
  #' @param object an S4 class object
  #' @export SinkingFund
  setGeneric("SinkingFund", function(object)
    {standardGeneric("SinkingFund")})

  setMethod("initialize",
          signature = ("BondDetails"),
          function(.Object,
                   Cusip = "character",
                   ID ="character",
                   BondType = "character",
                   Sector ="character",
                   Issuer = "character",
                   Underwriter = "character",
                   OfferAmount = "numeric",
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
                           Issuer = Issuer,
                           Underwriter = Underwriter,
                           OfferAmount = OfferAmount,
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
  
  #' Method to extract Cusip from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod Cusip
  setMethod("Cusip", signature("BondDetails"),
            function(object){object@Cusip})
  
  #' Method to extract ID from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object BondDetails
  #' @exportMethod ID
  setMethod("ID", signature("BondDetails"),
            function(object){object@ID})
  
  #' Method to extract BondType from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod BondType
  setMethod("BondType", signature("BondDetails"),
            function(object){object@BondType})
  
  #' Method to extract Sector from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod Sector
  setMethod("Sector", signature("BondDetails"),
            function(object){object@Sector})
  
  #' Method to extract Issuer from an S4 class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod Issuer
  setMethod("Issuer", signature("BondDetails"),
            function(object){object@Issuer})
  
  #' Method to extract Underwriter from an S$ class of type BondDetails
  #' 
  #' @param object The name of the S4 object of type BondDetails
  #' @exportMethod Underwriter
  setMethod("Underwriter", signature("BondDetails"),
            function(object){object@Underwriter})
  
  #' Method to get OfferAmount from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 object of the type BondDetails
  #' @exportMethod OfferAmount
  setMethod("OfferAmount", signature("BondDetails"),
            function(object){object@OfferAmount})
  
  #' Method to extract Coupon from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 object of type BondDetails
  #' @exportMethod Cusip
  setMethod("Coupon", signature("BondDetails"),
            function(object){object@Coupon})
  
  #' Method to extract IssueDate from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 class of type BondDetails
  #' @exportMethod IssueDate
  setMethod("IssueDate", signature("BondDetails"),
            function(object){object@IssueDate})
  
  #' Method to extract DatedDate from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 class of type BondDetails
  #' @exportMethod DatedDate
  setMethod("DatedDate", signature("BondDetails"),
            function(object){object@DatedDate})
  
  #' Method to extract Maturity from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 class of type BondDetails
  #' @exportMethod Maturity
  setMethod("Maturity", signature("BondDetails"),
            function(object){object@Maturity})
  
  #' Method to extract LastPmtDate from an S4 class of type BondDetails
  #' 
  #' @param object the name of the S4 class of type BondDetails
  #' @exportMethod LastPmtDate
  setMethod("LastPmtDate", signature("BondDetails"),
            function(object){object@LastPmtDate})
  
  #' Method to replace the slot LastPmtDate from class BondDetails
  #' 
  #' @param object the name of the S4 object of the type BondDetails
  #' @param value the value of the replacement
  #' @exportMethod LastPmtDate<-
  setReplaceMethod("LastPmtDate", signature("BondDetails"),
                   function(object, value){
                     object@LastPmtDate <- value
                     return(object)
                   })
  #' Method extract the slot NextPmtDate from class BondDetails
  #' 
  #' @param object the name of the S4 object of the type BondDetails
  #' @exportMethod NextPmtDate
  setMethod("NextPmtDate", signature("BondDetails"),
            function(object){object@NextPmtDate})
  
  #' Method to replace the slot NextPmtDate from class BondDetails
  #' 
  #' @param object the name of the S4 object of the type BondDetails
  #' @param value the value of the replacement
  #' @exportMethod NextPmtDate<-
  setReplaceMethod("NextPmtDate", signature("BondDetails"),
                   function(object, value){
                     object@NextPmtDate <- value
                     return(object)
                   })
  #' Method to extract Moody rating from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetails
  #' @exportMethod MoodyRating
  setMethod("MoodyRating", signature("BondDetails"),
            function(object){object@Moody})
  
  #' Method to extract SP rating from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetails
  #' @exportMethod SPRating
  setMethod("SPRating", signature("BondDetails"),
            function(object){object@SP})
  
  #' Method to extract BondLab rating from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetails
  #' @exportMethod BondLabRating
  setMethod("BondLabRating", signature("BondDetails"),
            function(object){object@BondLabRating})
  
  #' Method to extract the slot Frequency from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetails
  #' @exportMethod Frequency
  setMethod("Frequency", signature("BondDetails"),
            function(object){object@Frequency})
  
  #' Method to extract the slot BondBasis from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetails
  #' @exportMethod BondBasis
  setMethod("BondBasis", signature("BondDetails"),
            function(object){object@BondBasis})
  
  #' Method to extract the slot Callable from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetails
  #' @exportMethod Callable
  setMethod("Callable", signature("BondDetails"),
            function(object){object@Callable})
  
  #' Method to extract the slot Putable from S4 class of type BondDetails
  #' 
  #' @param object the name of the object of type BondDetials
  #' @exportMethod Putable
  setMethod("Putable", signature("BondDetails"),
            function(object){object@Putable})
  
  #' Method to extract the slot SinkingFund from S4 class of type BondDetails
  #' 
  #' @param object the name of hte object of type BondDetails
  #' @exportMethod SinkingFund
  setMethod("SinkingFund", signature("BondDetails"),
            function(object){object@SinkingFund})
  
  #' @title BondDetails
  #' @family BondDetails
  #' @description BondDetails creates a BondDetails cusip object in the 
  #' local environment
  #' @param Cusip A character the bond cusip
  #' @param ID a character the bond id
  #' @param BondType A character the type of bond MBS, Bond, etc.
  #' @param Sector A character the description of the sector
  #' @param Issuer A character the Issuer
  #' @param Underwriter A character the Underwriter
  #' @param OfferAmount A numeric value the initial offer amount
  #' @param Coupon A numeric value the bond coupon
  #' @param IssueDate A character the issue date mm-dd-YYYY
  #' @param DatedDate A character the dated date mm-dd-YYYY
  #' @param Maturity A character the maturity date mm-dd-YYYY
  #' @param LastPmtDate A character the last scheduled payment date to the
  #' investor mm-dd-YYYY
  #' @param NextPmtDate A character the next scheduled payment date to the 
  #' investor mm-dd-YYYY
  #' @param Moody A character the Moody Rating
  #' @param SP A character the SP Rating
  #' @param BondLab A character the BondLab (Investor) assigned rating
  #' @param Frequency A numeric value the frequency (number) of payments made
  #' each year
  #' @param BondBasis A character the interest calculation basis
  #' @param Callable A logical TRUE if the bond is callable
  #' @param Putable A logical TRUE if the bond is putable
  #' @param SinkingFund A logical TRUE if the bond is sinking fund bond
  #' @export BondDetails
  BondDetails <-function(
    Cusip,
    ID,
    BondType,
    Sector,
    Issuer,
    Underwriter,
    OfferAmount,
    Coupon,
    IssueDate,
    DatedDate,
    Maturity,
    LastPmtDate,
    NextPmtDate,
    Moody,
    SP,
    BondLab,
    Frequency,
    BondBasis,
    Callable,
    Putable,
    SinkingFund) {
    
    new("BondDetails",
        Cusip = Cusip,
        ID = ID,
        BondType = BondType,
        Sector = Sector,
        Issuer = Issuer,
        Underwriter = Underwriter,
        OfferAmount = OfferAmount,
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

  #' @title MakeBondDetails
  #' @family BondDetails
  #' @description MakeBondDetails writes a BondDetails cusip object to a datafolder
  #' @param Cusip A character the bond cusip
  #' @param ID a character the bond id
  #' @param BondType A character the type of bond MBS, Bond, etc.
  #' @param Sector A character the description of the sector
  #' @param Issuer A character the Issuer
  #' @param Underwriter A character the Underwriter
  #' @param OfferAmount A numeric value the initial offer amount
  #' @param Coupon A numeric value the bond coupon
  #' @param IssueDate A character the issue date mm-dd-YYYY
  #' @param DatedDate A character the dated date mm-dd-YYYY
  #' @param Maturity A character the maturity date mm-dd-YYYY
  #' @param LastPmtDate A character the last scheduled payment date to the
  #' investor mm-dd-YYYY
  #' @param NextPmtDate A character the next scheduled payment date to the 
  #' investor mm-dd-YYYY
  #' @param Moody A character the Moody Rating
  #' @param SP A character the SP Rating
  #' @param BondLab A character the BondLab (Investor) assigned rating
  #' @param Frequency A numeric value the frequency (number) of payments made
  #' each year
  #' @param BondBasis A character the interest calculation basis
  #' @param Callable A logical TRUE if the bond is callable
  #' @param Putable A logical TRUE if the bond is putable
  #' @param SinkingFund A logical TRUE if the bond is sinking fund bond
  #' @export MakeBondDetails
    MakeBondDetails <- function(    
    Cusip,
    ID,
    BondType,
    Sector,
    Issuer,
    Underwriter,
    OfferAmount,
    Coupon,
    IssueDate,
    DatedDate,
    Maturity,
    LastPmtDate,
    NextPmtDate,
    Moody,
    SP,
    BondLab,
    Frequency,
    BondBasis,
    Callable,
    Putable,
    SinkingFund)
    {
      temp <- BondDetails(
      Cusip = Cusip,
      ID = ID,
      BondType = BondType,
      Sector = Sector,
      Issuer = Issuer,
      Underwriter = Underwriter,
      OfferAmount = OfferAmount,
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
  }
