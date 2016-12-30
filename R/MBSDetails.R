
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
  

  #' An S4 class to represent a mortgage pass-through security
  #' 
  #' @slot Cusip A character string the pass-through cusip
  #' @slot ID A character string identifying the agency and pool number
  #' @slot BondType A character string the bond type
  #' @slot Issuer A character string the Issuer
  #' @slot Underwriter A character string the Underwriter
  #' @slot Sector A character string the sector
  #' @slot Coupon A coupon a numeric value the annual coupon rate
  #' @slot IssueDate A character string the issue date
  #' @slot DatedDate A character string the dated date
  #' @slot Maturity A character string the maturity date
  #' @slot LastPmtDate A character string the last payment advanced
  #' @slot NextPmtDate A character string the next payment date
  #' @slot Term A numeric value the original term of the underlying mortgage
  #' @slot WALA A numeric value the weighted average loan age
  #' @slot WAM A numeric value the weighted average maturity
  #' @slot PaymentDelay A numeric value the payment delay
  #' @slot Moody A character string the Moody credit rating
  #' @slot SP A character string the Standard and Poors credit rating
  #' @slot BondLab A character string the BondLab credit rating
  #' @slot Frequency A numeric value the frequency of payments 
  #' (annual = 1, semi-annual = 2, monthly = 12)
  #' @slot BondBasis A character string the day count used 
  #' ("actual360",30360", etc.)
  #' @slot GWac A numeric string the weighted average note rate
  #' @slot OrigLoanBal A numeric value the original loan balance
  #' @slot OrigLTV A numeric value the original loan to value ratio
  #' @slot AmortizationType A character string "fixed" or "arm"
  #' @slot AmortizationTerm A numeric value the term of the underlying mortgage
  #' @slot Index A character string the index to which the arm coupon is related
  #' @slot Margin A numeric value the spread (margin) over the index
  #' @slot FirstPmtDate A character string the mortgage first payment date
  #' @slot FinalPmtDate A character string the mortgage final payment date
  #' @slot Servicing A numeric value the serving strip
  #' @slot PMI A numeric value the primary mortgage insurance paid
  #' @slot GFeePremium A numeric value the guarantee fee paid
  #' @slot InitialInterest A logical value TRUE or FALSE interest only mortgage
  #' @slot InterestOnlyPeriod A numeric value the number of months the 
  #' borrower pays only interest
  #' @slot FirstPrinPaymentDate A character string the date of the 
  #' first principal payment
  #' @slot BalloonPmt A logical value TRUE or FALSE a balloon payment due
  #' @slot BalloonDate A character string the balloon payment date
  #' @slot MBSFactor A numeric value the pass-through principal balance 
  #' outstanding expressed as percent of the original balance
  #' @slot OriginalBal A numeric value the pass-though original balance
  #' @slot CurrentBal A numeric value the pass-through current balance 
  #' a percentage of the original balance
  #' @slot Model A character string the prepayment model tune file
  #' @slot Burnout A numeric value the burnout value
  #' @slot SATO A numeric value the mortgage rate spread at origination
  #' @exportClass MBSDetails   
  setClass("MBSDetails", 
         representation(
           Cusip = "character",
           ID = "character",
           BondType = "character",
           Sector ="character",
           Issuer = "character",
           Underwriter = "character",
           Coupon = "numeric",
           IssueDate = "character",
           DatedDate = "character",
           Maturity = "character",
           LastPmtDate = "character",
           NextPmtDate = "character",
           Term = "numeric",
           WALA = "numeric",
           WAM = "numeric",
           PaymentDelay = "numeric",
           Moody = "character",
           SP = "character",
           BondLab  = "character",
           Frequency = "numeric",
           BondBasis = "character",
           GWac = "numeric",
           OrigLoanBal = "numeric",
           OrigLTV = "numeric",
           AmortizationType = "character",
           AmortizationTerm = "numeric",
           Index = "character",
           Margin = "numeric",
           FirstPmtDate = "character",
           FinalPmtDate = "character",
           Servicing = "numeric",
           PMI = "numeric",
           GFeePremium = "numeric",
           InitialInterest = "logical",
           InterestOnlyPeriod = "numeric",
           FirstPrinPaymentDate = "character",
           BalloonPmt = "logical",
           BalloonDate = "character",
           MBSFactor = "numeric",
           OriginalBal = "numeric",
           CurrentBal = "numeric",
           Model = "character",
           Burnout = "numeric",
           SATO = "numeric"))

  setGeneric("MBSDetails", function(
    Cusip = "character",
    ID = "character",
    BondType = "character",
    Issuer = "character",
    Underwriter = "character",
    Sector ="character",
    Coupon = numeric(),
    IssueDate = "character",
    DatedDate = "character",
    Maturity = "character",
    LastPmtDate = "character",
    NextPmtDate = "character",
    Term = numeric(),
    WALA = numeric(),
    WAM = numeric(),
    PaymentDelay = numeric(),
    Moody = "character",
    SP = "character",
    BondLab  = "character",
    Frequency = numeric(),
    BondBasis = "character",
    GWac = numeric(),
    OrigLoanBal = numeric(),
    OrigLTV = numeric(),
    AmortizationType = "character",
    AmortizationTerm = numeric(),
    Index = "character",
    Margin = numeric(),
    FirstPmtDate = "character",
    FinalPmtDate = "character",
    Servicing = numeric(),
    PMI = numeric(),
    GFeePremium = numeric(),
    InitialInterest = "logical",
    InterestOnlyPeriod = numeric(),
    FirstPrinPaymentDate = "character",
    BalloonPmt = "logical",
    BalloonDate = "character",
    MBSFactor = numeric(),
    OriginalBal = numeric(),
    CurrentBal = numeric(),
    Model = "character",
    Burnout = numeric(),
    SATO = numeric())
  
  {standardGeneric("MBSDetails")})
  
  #' A standard generic function to access the slot Cusip
  #' @param object an S4 class object
  #' @export Cusip
  setGeneric("Cusip", function(object)
    {standardGeneric("Cusip")})
  
  #' A standard generic function to access the slot ID
  #' @param object an S4 class object
  #' @export ID
  setGeneric("ID", function(object)
    {standardGeneric("ID")})
  
  #' A standard generic function to access the slot BondType
  #' @param object an S4 class object
  #' @export BondType
  setGeneric("BondType", function(object)
    {standardGeneric("BondType")})
  
  #' A standard generic function to access the slot Sector
  #' @param object and S4 class object
  #' @export Sector
  setGeneric("Sector", function(object)
    {standardGeneric("Sector")})
  
  #' A generic function to access the slot Issuer
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("Issuer", function(object)
  {standardGeneric("Issuer")})
  
  #' A generic function to access the slot Underwriter
  #' 
  #' @param object an S4 class object
  #' @export
  setGeneric("Underwriter", function(object)
  {standardGeneric("Underwriter")})
  
  
  #' A standard generic function to access the slot Coupon
  #' @param object an S4 class object
  #' @export Coupon
  setGeneric("Coupon", function(object)
    {standardGeneric("Coupon")})
  
  #' A standard generic function to access the slot IssueDate
  #' @param object an S4 class object
  #' @export IssueDate
  setGeneric("IssueDate", function(object)
    {standardGeneric("IssueDate")})
  
  #' A standard generic function to access the slot DatedDate
  #' @param object an S4 class object
  #' @export DatedDate
  setGeneric("DatedDate", function(object)
    {standardGeneric("DatedDate")})
  
  #' A standard generic function to access the slot Maturity
  #' @param object an S4 class object
  #' @export Maturity
  setGeneric("Maturity", function(object)
    {standardGeneric("Maturity")})
  
  #' A standard generic function to access the slot LastPmtDate
  #' @param object an S4 class object
  #' @export LastPmtDate
  setGeneric("LastPmtDate", function(object)
    {standardGeneric("LastPmtDate")})
  
  #' A standard generic function to replace the slot LastPmtDate
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export LastPmtDate<-
  setGeneric("LastPmtDate<-", function(object, value)
  {standardGeneric("LastPmtDate<-")})
  
  #' A standard generic function to access the slot NextPmtDate
  #' @param  object an S4 class object
  #' @export NextPmtDate
  setGeneric("NextPmtDate", function(object)
    {standardGeneric("NextPmtDate")})
  
  #' A standard generic function to replace the slot NextPmtDate
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export NextPmtDate<-
  setGeneric("NextPmtDate<-", function(object, value)
    {standardGeneric("NextPmtDate<-")})
  
  #' A standard generic function to access the slot Term
  #' @param object an S4 class object
  #' @export Term
  setGeneric("Term", function(object)
    {standardGeneric("Term")})
  
  #' A standard generic function to access the slot WALA
  #' @param object an S4 class object
  #' @export WALA
  setGeneric("WALA", function(object)
    {standardGeneric("WALA")})
  
  #' A standard generic function to replace the slot WALA
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export WALA<-
  setGeneric("WALA<-", function(object, value)
    {standardGeneric("WALA<-")})
  
  #' A standard generic function to access the slot WAM
  #' @param object an S4 class object
  #' @export WAM 
  setGeneric("WAM", function(object)
    {standardGeneric("WAM")})
  
  #' A standard generic function to replace the slot WAM
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export WAM<-
  setGeneric("WAM<-", function(object, value)
    {standardGeneric("WAM<-")})
  
  #' A standard generic function to access the slot PaymentDelay
  #' @param object an S4 class object
  #' @export PaymentDelay
  setGeneric("PaymentDelay", function(object)
    {standardGeneric("PaymentDelay")})
  
  #' A standard generic function to access the slot Moody
  #' @param object an S4 class object
  #' @export MoodyRating
  setGeneric("MoodyRating", function(object)
    {standardGeneric("MoodyRating")})

  #' A standard generic function to access the slot SP
  #' @param object an S4 class object
  #' @export SPRating
  setGeneric("SPRating", function(object)
    {standardGeneric("SPRating")})
  
  #' A standard generic function to access the slot BondLab
  #' @param object an S4 class object
  #' @export BondLabRating
  setGeneric("BondLabRating", function(object)
    standardGeneric("BondLabRating"))
  
  #' A standard generic function to access the slot Frequency
  #' @param object an S4 class object
  #' @export Frequency
  setGeneric("Frequency", function(object)
    {standardGeneric("Frequency")})
  
  #' A standard generic function to access the slot BondBasis
  #' @param object an S4 class object
  #' @export BondBasis
  setGeneric("BondBasis", function(object)
    {standardGeneric("BondBasis")})
  
  #' A standard generic function to access the slot GWac
  #' @param object an S4 class object
  #' @export GWac
  setGeneric("GWac", function(object)
    {standardGeneric("GWac")})
  
  #' A standard generic function to access the slot OrigLoanBal
  #' @param object an S4 class object
  #' @export OrigLoanBal
  setGeneric("OrigLoanBal", function(object)
    {standardGeneric("OrigLoanBal")})
  
  #' A standard generic function to access the slot OrigLTV
  #' @param object an S4 class object
  #' @export OrigLTV
  setGeneric("OrigLTV", function(object)
    {standardGeneric("OrigLTV")})
  
  #' A standard generic function to access the slot AmortizationType
  #' @param object an S4 class object
  #' @export AmortizationType
  setGeneric("AmortizationType", function(object)
    {standardGeneric("AmortizationType")})
  
  #' A standard generic function to access the slot AmortizationTerm
  #' @param object an S4 class object
  #' @export AmortizationTerm
  setGeneric("AmortizationTerm", function(object)
    {standardGeneric("AmortizationTerm")})
  
  #' A standard generic function to access the slot Index
  #' @param object an S4 class object
  #' @export Index
  setGeneric("Index", function(object)
    {standardGeneric("Index")})
  
  #' A standard generic function to access the slot Margin
  #' @param object an S4 class object
  #' @export Margin
  setGeneric("Margin", function(object)
    {standardGeneric("Margin")})
  
  #' A standard generic function to access the slot FirstPmtDate
  #' @param object an S4 class object
  #' @export FirstPmtDate
  setGeneric("FirstPmtDate", function(object)
    {standardGeneric("FirstPmtDate")})
  
  #' A standard generic function to access the slot FinalPmtDate
  #' @param object an S4 class object
  #' @export FinalPmtDate
  setGeneric("FinalPmtDate", function(object)
    {standardGeneric("FinalPmtDate")})
  
  #' A standard generic function to access the slot BalloonPmt
  #' @param object S4 class object
  #' @export Servicing
  setGeneric("Servicing", function(object)
    {standardGeneric("Servicing")})
  
  #' A standard generic function to access the slot PMI
  #' @param object S4 class object
  #' @export PMI
  setGeneric("PMI", function(object)
    {standardGeneric("PMI")})
  
  #' A standard generic function to access the slot of GFeePremium
  #' @param object an S4 class object of the type MortgageCashFlow
  #' @export GFeePremium
  setGeneric("GFeePremium", function(object)
  {standardGeneric})
  
  #' A standard generic function to access the slot InitialInterest
  #' @param object S4 class object
  #' @export InitialInterest
  setGeneric("InitialInterest", function(object)
    {standardGeneric("InitialInterest")})
  
  #' A standard generic function to access the slot InterestOnlyPeriod
  #' @param object S4 class object
  #' @export InterestOnlyPeriod
  setGeneric("InterestOnlyPeriod", function(object)
    {standardGeneric("InterestOnlyPeriod")})
  
  #' A standard generic function to access the slot FirstPrinPaymentDate
  #' @param object S4 class object
  #' @export FirstPrinPaymentDate
  setGeneric("FirstPrinPaymentDate", function(object)
    {standardGeneric("FirstPrinPaymentDate")})
  
  #' A standard generic function to access the slot BalloonPmt
  #' @param object an S4 class object
  #' @export BalloonPmt
  setGeneric("BalloonPmt", function(object)
    {standardGeneric("BalloonPmt")})
  
  #' A standard generic function to access the slot BalloonDate
  #' @param object an S4 class object
  #' @export BalloonDate
  setGeneric("BalloonDate", function(object)
    {standardGeneric("BalloonDate")})
  
  #' A standard generic function to access the slot MBSFactor
  #' @param object an S4 class object
  #' @export MBSFactor
  setGeneric("MBSFactor", function(object)
    {standardGeneric("MBSFactor")})
  
  #' A standard generic function to replace the slot MBSFactor
  #' @param object an S4 class object
  #' @param value the replacement value
  #' @export MBSFactor<-
  setGeneric("MBSFactor<-", function(object, value)
    {setGeneric("MBSFactor<-")})
  
  #' A standard generic function to access the slot OriginalBal
  #' @param object an S4 class object
  #' @export OriginalBal
  setGeneric("OriginalBal", function(object)
    {standardGeneric("OriginalBal")})
  
  #' A standard generic function to access the slot CurrentBal
  #' @param object an S4 class object
  #' @export CurrentBal
  setGeneric("CurrentBal", function(object)
    {standardGeneric("CurrentBal")})
  
  #' A standard generic function to replace the slot CurrentBal
  #' @param object an S4 class object
  #' @param value the replacement value of the slot
  #' @export CurrentBal<-
  setGeneric("CurrentBal<-", function(object, value)
    {standardGeneric("CurrentBal<-")})
  
  #' A standard generic function to access the slot Model
  #' @param object an S4 class object
  #' @export Model
  setGeneric("Model", function(object)
    {standardGeneric("Model")})

  #' A standard generic function to access the slot Burnout
  #' @param object an S4 class object
  #' @export BurnOut
  setGeneric("BurnOut", function(object)
  {standardGeneric("BurnOut")})
  
  #' A standard generic function to access the slot SATO
  #' @param object an S4 class object
  #' @export SATO
  setGeneric("SATO", function(object)
    {standardGeneric("SATO")})
  
  setMethod("initialize",
          signature("MBSDetails"),
          function(.Object,
                   Cusip = "character",
                   ID = "character",
                   BondType = "character",
                   Issuer = "character",
                   Underwriter = "character",
                   Sector ="character",
                   Coupon = "numeric",
                   IssueDate = "character",
                   DatedDate = "character",
                   Maturity = "character",
                   LastPmtDate = "character",
                   NextPmtDate = "character",
                   Term = "numeric",
                   WALA = "numeric",
                   WAM = "numeric",
                   PaymentDelay = "numeric",
                   Moody = "character",
                   SP = "character",
                   BondLab  = "character",
                   Frequency = "numeric",
                   BondBasis = "character",
                   GWac = "numeric",
                   OrigLoanBal = "numeric",
                   OrigLTV = "numeric",
                   AmortizationType = "character",
                   AmortizationTerm = "numeric",
                   Index = "character",
                   Margin = "numeric",
                   FirstPmtDate = "character",
                   FinalPmtDate = "character",
                   Servicing = "numeric",
                   PMI = "numeric",
                   GFeePremium = "numeric",
                   InitialInterest = "logical",
                   InterestOnlyPeriod = "numeric",
                   FirstPrinPaymentDate = "character",
                   BalloonPmt = "logical",
                   BalloonDate = "character",
                   MBSFactor = "numeric",
                   OriginalBal = "numeric",
                   CurrentBal = "numeric",
                   Model = "character",
                   Burnout = "numeric",
                   SATO = "numeric",
                   ...){
            callNextMethod(.Object,
                           Cusip = Cusip,
                           ID = ID,
                           BondType = BondType,
                           Issuer = Issuer,
                           Underwriter = Underwriter,
                           Sector = Sector,
                           Coupon = Coupon,
                           IssueDate = IssueDate,
                           DatedDate = DatedDate,
                           Maturity = Maturity,
                           LastPmtDate = LastPmtDate,
                           NextPmtDate = NextPmtDate,
                           Term = Term,
                           WALA = WALA,
                           WAM = WAM,
                           PaymentDelay = PaymentDelay,
                           Moody = Moody,
                           SP = SP,
                           BondLab = BondLab,
                           Frequency = Frequency,
                           BondBasis = BondBasis,
                           GWac = GWac,
                           OrigLoanBal = OrigLoanBal,
                           OrigLTV = OrigLTV,
                           AmortizationType = AmortizationType,
                           AmortizationTerm = AmortizationTerm,
                           Index = Index,
                           Margin = Margin,
                           FirstPmtDate = FirstPmtDate,
                           FinalPmtDate = FinalPmtDate,
                           Servicing = Servicing,
                           PMI = PMI,
                           GFeePremium = GFeePremium,
                           InitialInterest = InitialInterest,
                           InterestOnlyPeriod = InterestOnlyPeriod,
                           FirstPrinPaymentDate = FirstPrinPaymentDate,
                           BalloonPmt = BalloonPmt,
                           BalloonDate = BalloonDate,
                           MBSFactor = MBSFactor,
                           OriginalBal = OriginalBal,
                           CurrentBal = CurrentBal,
                           Model = Model,
                           Burnout = Burnout,
                           SATO = SATO,
            ...)
          })

  #' Method to extract the slot Cusip from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Cusip 
  setMethod("Cusip", signature("MBSDetails"),
            function(object){object@Cusip})
  
  #' Method to extract the slot ID from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod ID
  setMethod("ID", signature("MBSDetails"),
            function(object){object@ID})
  
  #' Method to extract the slot BondType from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod BondType
  setMethod("BondType", signature("MBSDetails"),
            function(object){object@BondType})
  
  #' Method to extreact the slot Issuer from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Issuer
  setMethod("Issuer", signature("MBSDetails"),
            function(object){object@Issuer})
  
  #' Method to extract the slot Underwriter from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Underwriter
  setMethod("Underwriter", signature("MBSDetails"),
            function(object){object@Underwriter})
  
  #' Method to extract the slot Sector from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Sector
  setMethod("Sector", signature("MBSDetails"),
            function(object){object@Sector})
  
  #' Method to extract the slot Coupon from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Coupon
  setMethod("Coupon", signature("MBSDetails"),
            function(object){object@Coupon})
  
  #' Method to extract the slot IssueDate from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod IssueDate
  setMethod("IssueDate", signature("MBSDetails"),
            function(object){object@IssueDate})
  
  #' Method to extract the slot DatedDate from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod DatedDate
  setMethod("DatedDate", signature("MBSDetails"),
            function(object){object@DatedDate})
  
  #' Method to extract the slot Maturity from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Maturity
  setMethod("Maturity", signature("MBSDetails"),
            function(object){object@Maturity})
  
  #' Method to extract the slot LastPmtDate from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod LastPmtDate
  setMethod("LastPmtDate", signature("MBSDetails"),
            function(object){object@LastPmtDate})
  
  #' Method to replace slot LastPmtDate from in class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @param value the value of the replacement
  #' @exportMethod LastPmtDate<-
  setReplaceMethod("LastPmtDate", signature("MBSDetails"),
                   function(object, value){
                     object@LastPmtDate <- value
                     return(object)
                   })
  
  #' Method to extract the slot NextPmtDate from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod NextPmtDate
  setMethod("NextPmtDate", signature("MBSDetails"),
            function(object){object@NextPmtDate})
  
  #' Method to extract the slot NextPmtDate from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @param value the value of the slot replacement
  #' @exportMethod NextPmtDate<-
  setReplaceMethod("NextPmtDate", signature("MBSDetails"),
                   function(object, value){
                     object@NextPmtDate <- value
                     return(object)
                   })
  
  #' Method to extract the slot Term from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod Term
  setMethod("Term", signature("MBSDetails"),
            function(object){object@Term})
  
  #' Method to extract the slot WALA from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod WALA
  setMethod("WALA", signature("MBSDetails"),
            function(object){object@WALA})
  
  #' Method to replace the slot WALA in the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @param value the replacement value of the slot 
  #' @exportMethod WALA<-
  setReplaceMethod("WALA", signature("MBSDetails"),
                   function(object, value){
                     object@WALA <- value
                     return(object)
                   })
  
  #' Method to extract the slot PaymentDelay from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod PaymentDelay
  setMethod("PaymentDelay", signature("MBSDetails"),
            function(object){object@PaymentDelay})
  
  #' Method to extract the slot WAM from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod WAM
  setMethod("WAM", signature("MBSDetails"),
            function(object){object@WAM})
  
  #' Method to replace the slot WAM in the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @param value the replacement value of the slot
  #' @exportMethod WAM<-
  setReplaceMethod("WAM", signature("MBSDetails"),
                   function(object, value){
                     object@WAM <- value
                     return(object)
                   })
  
  #' Method to extract the slot Moody from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod MoodyRating
  setMethod("MoodyRating", signature("MBSDetails"),
            function(object){object@Moody})
  
  #' Method to extract the slot SP from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod SPRating
  setMethod("SPRating", signature("MBSDetails"),
            function(object){object@SP})
  
  #' Method to extract the slot BondLab from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod BondLabRating
  setMethod("BondLabRating", signature("MBSDetails"),
            function(object){object@BondLab})
  
  #' Method to extract the slot Frequency from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod Frequency
  setMethod("Frequency", signature("MBSDetails"),
            function(object){object@Frequency})
  
  #' Method to extract the slot BondBasis from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod BondBasis
  setMethod("BondBasis", signature("MBSDetails"),
            function(object){object@BondBasis})
  
  #' Method to extract the slot GWac from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod GWac
  setMethod("GWac", signature("MBSDetails"),
            function(object){object@GWac})
  
  #' Method to extract the slot OrigLoanBal from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod OrigLoanBal
  setMethod("OrigLoanBal", signature("MBSDetails"),
            function(object){object@OrigLoanBal})
  
  #' Method to extract the slot OrigLTV from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod OrigLTV
  setMethod("OrigLTV", signature("MBSDetails"),
            function(object){object@OrigLTV})
  
  #' Method to extract the slot AmortizationType from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod AmortizationType
  setMethod("AmortizationType", signature("MBSDetails"),
            function(object){object@AmortizationType})
  
  #' Method to extract the slot Amortization Term from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod AmortizationTerm
  setMethod("AmortizationTerm", signature("MBSDetails"),
            function(object){object@AmortizationTerm})
  
  #' Method to extract the slot Index from the class MBS Details
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Index
  setMethod("Index", signature("MBSDetails"),
            function(object){object@Index})
  
  #' Method to extract the slot Margin from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod Margin
  setMethod("Margin", signature("MBSDetails"),
            function(object){object@Margin})
  
  #' Method to extract the slot FirstPmtDate from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod FirstPmtDate
  setMethod("FirstPmtDate", signature("MBSDetails"),
            function(object){object@FirstPmtDate})
  
  #' Method to extract the slot FinalPmtDate from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @exportMethod FinalPmtDate
  setMethod("FinalPmtDate", signature("MBSDetails"),
            function(object){object@FinalPmtDate})
  
  #' Method to extract the slot Servicing from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod Servicing
  setMethod("Servicing", signature("MBSDetails"),
            function(object){object@Servicing})
  
  #' Method to extract the slot PMI from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod PMI
  setMethod("PMI", signature("MBSDetails"),
            function(object){object@PMI})
  
  #' Method to extract the slot GFee from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod GFeePremium
  setMethod("GFeePremium", signature("MBSDetails"),
            function(object){object@GFeePremium})
  
  #' Method to extract the slot InitialInterest from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod InitialInterest
  setMethod("InitialInterest", signature("MBSDetails"),
            function(object){object@InitialInterest})
  
  #' Method to extract the slot InterestOnlyPeriod from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod InterestOnlyPeriod
  setMethod("InterestOnlyPeriod", signature("MBSDetails"),
            function(object){object@InterestOnlyPeriod})
  
  #' Method to extract the slot FirstPrinPaymentDate from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod FirstPrinPaymentDate
  setMethod("FirstPrinPaymentDate", signature("MBSDetails"),
            function(object){object@FirstPrinPaymentDate})
  
  #' Method to extract the slot BalloonPmt from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod BalloonPmt
  setMethod("BalloonPmt", signature("MBSDetails"),
            function(object){object@BalloonPmt})
  
  #' Method to extract the slot BalloonDate from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod BalloonDate
  setMethod("BalloonDate", signature("MBSDetails"),
            function(object){object@BalloonDate})
  
  #' Method to extract the slot MBSFactor from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod MBSFactor
  setMethod("MBSFactor", signature("MBSDetails"),
            function(object){object@MBSFactor})
  
  #' Method to replace the slot MBSFactor from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @param value the replacement value of the slot
  #' @exportMethod MBSFactor<-
  setReplaceMethod("MBSFactor", signature("MBSDetails"),
                   function(object, value){
                     object@MBSFactor <- value
                     return(object)
                     })
  
  #' Method to extract the slot OriginalBal from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod OriginalBal
  setMethod("OriginalBal", signature("MBSDetails"),
            function(object){object@OriginalBal})
  
  #' Method to extract the slot CurrentBal from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod CurrentBal
  setMethod("CurrentBal", signature("MBSDetails"),
            function(object){object@CurrentBal})
  
  #' Method to replace the slot CurrentBal from the class MBSDetails
  #' @param object the name of the object of type MBSDetails
  #' @param value the replacement value of the slot
  #' @exportMethod CurrentBal<-
  setReplaceMethod("CurrentBal", signature("MBSDetails"),
                   function(object, value){
                     object@CurrentBal <- value
                     return(object)
                   })
  
  #' Method to extract the slot Model from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod Model
  setMethod("Model", signature("MBSDetails"),
            function(object){object@Model})
  
  #' Mehthod to extract the slot Burnout from the class MBSDetails
  #' @param object the name of the object of the type MBSDetails
  #' @exportMethod BurnOut
  setMethod("BurnOut", signature("MBSDetails"),
            function(object){object@Burnout})
  
  #' Method to extract the slot SATO from the class MBSDetails
  #' @param object the name of the object of the tyoe MBSDetails
  #' @exportMethod SATO
  setMethod("SATO", signature("MBSDetails"),
            function(object){object@SATO})
  
  #' @title MBSDetails
  #' @family MBSDetails
  #' @description MBSDetails creates an MBS pass-through cusip object in the 
  #' local environment 
  #' @param Cusip A character the Pass Through MBS cusip.
  #' @param ID A character string the pool number. 
  #' @param BondType A character string the type of Bond MBS, etc.
  #' @param Sector A character string description of the Sector Mortgage Sector
  #' @param Issuer A character string the Issuer
  #' @param Underwriter A character string the Underwriter 
  #' @param Coupon A numeric value the Bond Coupon.
  #' @param IssueDate A character string the issue date of the security.
  #' @param DatedDate A character sting The date following the issue when 
  #' interest begins to accure.
  #' @param Maturity A character sting the final payment date to the investor
  #' in the case MBS the final payment data assuming 0 CPR.
  #' @param LastPmtDate A character string the date the last payment scheduled 
  #' payment to the investor.
  #' @param NextPmtDate A character string the date of the next scheduled 
  #' payment to the investor.
  #' @param Term A numeric value the original term of the underlying mortgages
  #' @param WALA A numeric value the weighted average loan age of the 
  #' underlying mortgages
  #' @param WAM A numeric value the weighted average maturity of the 
  #' underlying mortgages
  #' @param PaymentDelay A numeric value in the case of MBS the delay of the 
  #' payment from the trust to the investor
  #' @param Moody A character string Moody's assigned credit rating
  #' @param SP A character string SP's assigned credit rating
  #' @param BondLab A character string BondLab's or the user's 
  #' assigned credit rating
  #' @param Frequency A numeric value string the frequency of payments made 
  #' to the investor
  #' @param BondBasis A character string the basis on which 
  #' interest is calculated
  #' @param GWac A numeric value the borrower's note rate
  #' @param OrigLoanBal A numeric value the original balance of the loan
  #' @param OrigLTV A numeric value the borrower's original loan to value ratio
  #' @param AmortizationType A character sting the type of the 
  #' loan 'fixed' or 'arm'.
  #' These values are used by the prepayment model to drive the mortgage 
  #' rate either fixed or adjustable mortgage rate
  #' @param AmortizationTerm A numeric value the term of the loan in years
  #' @param Index A character string if the amortization type is 
  #' adjustable the Index to which the note rate references
  #' @param Margin A numeric value the spread over the index used to determine 
  #' the borrower's note rate
  #' @param FirstPmtDate A character string the date of the first payment 
  #' of the borrower's note.
  #' @param FinalPmtDate A character string the date of thee final payment 
  #' of the borrower's note.  In the case of an MBS the final payment made to 
  #' the bondholder
  #' @param Servicing A numeric value the servicing spread from the Gross WAC 
  #' to the servicer
  #' of the mortgage pool's underlying loans.
  #' @param PMI A numeric value the primary mortage insurance paid by the 
  #' borrower to the PMI provider
  #' @param GFeePremium A numeric value the guarantee fee taken from the 
  #' borrower's note rate to guarantee timely payment of principal and interest.  
  #' Applicable in the case of Fannie Mae, Freddie Mac, or Ginnie Mae pools.
  #' @param InitialInterest A logical indicating the note carries an interest 
  #' only period
  #' @param InterestOnlyPeriod A character string indicating the note's 
  #' interest only period
  #' @param FirstPrinPaymentDate A character string indicating the first 
  #' principal payment date due of the mortgage.
  #' @param BalloonPmt A logical indicating the mortgage carries a balloon pmt.
  #' @param BalloonDate A character string the balloon payment date.
  #' @param MBSFactor A numeric value the current factor of the MBS.
  #' @param OriginalBal A numeric value the original balance of the MBS.
  #' @param CurrentBal A numeric value the current balance of the MBS.
  #' @param Model A character string the prepayment model to use.
  #' @param Burnout A numeric model the value of the borrower burnout.
  #' @param SATO A numeric value the borrrowers Spread AT Origination over the
  #' @export
  MBSDetails <- function(
  Cusip = "character",
  ID = "character",
  BondType = "character",
  Issuer = "character",
  Underwriter = "character",
  Sector ="character",
  Coupon = numeric(),
  IssueDate = "character",
  DatedDate = "character",
  Maturity = "character",
  LastPmtDate = "character",
  NextPmtDate = "character",
  Term = numeric(),
  WALA = numeric(),
  WAM = numeric(),
  PaymentDelay = numeric(),
  Moody = "character",
  SP = "character",
  BondLab  = "character",
  Frequency = numeric(),
  BondBasis = "character",
  GWac = numeric(),
  OrigLoanBal = numeric(),
  OrigLTV = numeric(),
  AmortizationType = "character",
  AmortizationTerm = numeric(),
  Index = "character",
  Margin = numeric(),
  FirstPmtDate = "character",
  FinalPmtDate = "character",
  Servicing = numeric(),
  PMI = numeric(),
  GFeePremium = numeric(),
  InitialInterest = "logical",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "logical",
  BalloonDate = "character",
  MBSFactor = numeric(),
  OriginalBal = numeric(),
  CurrentBal = numeric(),
  Model = "character",
  Burnout = numeric(),
  SATO = numeric()
  ) {
  
  new("MBSDetails",
      Cusip = Cusip,
      ID = ID,
      BondType = BondType,
      Issuer = Issuer,
      Underwriter = Underwriter,
      Sector = Sector,
      Coupon = Coupon,
      IssueDate = IssueDate,
      DatedDate = DatedDate,
      Maturity = Maturity,
      LastPmtDate = LastPmtDate,
      NextPmtDate = NextPmtDate,
      Term = Term,
      WALA = WALA,
      WAM = WAM,
      PaymentDelay = PaymentDelay,
      Moody = Moody,
      SP = SP,
      BondLab  = BondLab,
      Frequency = Frequency,
      BondBasis = BondBasis,
      GWac = GWac,
      OrigLoanBal = OrigLoanBal,
      OrigLTV = OrigLTV,
      AmortizationType = AmortizationType,
      AmortizationTerm = AmortizationTerm,
      Index = Index,
      Margin = Margin,
      FirstPmtDate = FirstPmtDate,
      FinalPmtDate = FinalPmtDate,
      Servicing = Servicing,
      PMI = PMI,
      GFeePremium = GFeePremium,
      InitialInterest = InitialInterest,
      InterestOnlyPeriod = InterestOnlyPeriod,
      FirstPrinPaymentDate = FirstPrinPaymentDate,
      BalloonPmt = BalloonPmt,
      BalloonDate = BalloonDate,
      MBSFactor = MBSFactor,
      OriginalBal = OriginalBal,
      CurrentBal = CurrentBal,
      Model = Model,
      Burnout = Burnout,
      SATO = SATO)
  }

  #' @title MakeMBSDetails
  #' @family MBSDetails
  #' @description The function calls MBSDetails function and saves the MBS
  #' cusip detail to the folder BondData.  This function is useful for working
  #' on a local install of BondLab with no database when the user would like to 
  #' repeatedly call the cusip.
  #' @param Cusip A character the Pass Through MBS cusip.
  #' @param ID A character string the pool number. 
  #' @param BondType A character string the type of Bond MBS, etc.
  #' @param Issuer A character string the Issuer
  #' @param Underwriter A character string the Underwriter 
  #' @param Sector A character string description of the Sector Mortgage Sector
  #' @param Coupon A numeric value the Bond Coupon.
  #' @param IssueDate A character string the issue date of the security.
  #' @param DatedDate A character sting The date following the issue when 
  #' interest begins to accure.
  #' @param Maturity A character sting the final payment date to the investor
  #' in the case MBS the final payment data assuming 0 CPR.
  #' @param LastPmtDate A character string the date the last payment scheduled 
  #' payment to the investor.
  #' @param NextPmtDate A character string the date of the next scheduled 
  #' payment to the investor.
  #' @param Term A numeric value the original term of the underlying mortgages
  #' @param WALA A numeric value the weighted average loan age of the 
  #' underlying mortgages
  #' @param WAM A numeric value the weighted average maturity of the 
  #' underlying mortgages
  #' @param PaymentDelay A numeric value in the case of MBS the delay of the 
  #' payment from the trust to the investor
  #' @param Moody A character string Moody's assigned credit rating
  #' @param SP A character string SP's assigned credit rating
  #' @param BondLab A character string BondLab's or the user's 
  #' assigned credit rating
  #' @param Frequency A numeric value string the frequency of payments made 
  #' to the investor
  #' @param BondBasis A character string the basis on which 
  #' interest is calculated
  #' @param GWac A numeric value the borrower's note rate
  #' @param OrigLoanBal A numeric value the original balance of the loan
  #' @param OrigLTV A numeric value the borrower's original loan to value ratio
  #' @param AmortizationType A character sting the type of the 
  #' loan 'fixed' or 'arm'.
  #' These values are used by the prepayment model to drive the mortgage 
  #' rate either fixed or adjustable mortgage rate
  #' @param AmortizationTerm A numeric value the term of the loan in years
  #' @param Index A character string if the amortization type is 
  #' adjustable the Index to which the note rate references
  #' @param Margin A numeric value the spread over the index used to determine 
  #' the borrower's note rate
  #' @param FirstPmtDate A character string the date of the first payment 
  #' of the borrower's note.
  #' @param FinalPmtDate A character string the date of thee final payment 
  #' of the borrower's note.  In the case of an MBS the final payment made to 
  #' the bondholder
  #' @param Servicing A numeric value the servicing spread from the Gross WAC 
  #' to the servicer
  #' of the mortgage pool's underlying loans.
  #' @param PMI A numeric value the primary mortage insurance paid by the 
  #' borrower to the PMI provider
  #' @param GFeePremium A numeric value the guarantee fee taken from the 
  #' borrower's note rate to guarantee timely payment of principal and interest.  
  #' Applicable in the case of Fannie Mae, Freddie Mac, or Ginnie Mae pools.
  #' @param InitialInterest A logical indicating the note carries an interest 
  #' only period
  #' @param InterestOnlyPeriod A character string indicating the note's 
  #' interest only period
  #' @param FirstPrinPaymentDate A character string indicating the first 
  #' principal payment date due of the mortgage.
  #' @param BalloonPmt A logical indicating the mortgage carries a balloon pmt.
  #' @param BalloonDate A character string the balloon payment date.
  #' @param MBSFactor A numeric value the current factor of the MBS.
  #' @param OriginalBal A numeric value the original balance of the MBS.
  #' @param CurrentBal A numeric value the current balance of the MBS.
  #' @param Model A character string the prepayment model to use.
  #' @param Burnout A numeric model the value of the borrower burnout.
  #' @param SATO A numeric value the borrrowers Spread AT Origination over the 
  #' prime lending rate.
  #' @examples 
  #' \dontrun{
  #'  MakeMBSDetails( 
  #'  Cusip = "23456789",
  #'  ID = "bondlabMBS4",
  #'  BondType = "MBS",
  #'  Issuer = FNMA,
  #'  Underwriter = FNMA,
  #'  Sector = "MBS",
  #'  Coupon = 4.0,
  #'  IssueDate = "01-01-2013",
  #'  DatedDate = "01-01-2013",
  #'  Maturity = "01-01-2043",
  #'  LastPmtDate = "01-01-2013",
  #'  NextPmtDate = "02-01-2013",
  #'  Term = 360,
  #'  WALA = 0,
  #'  WAM = 360,
  #'  PaymentDelay = 24,     
  #'  Moody = "Aaa",
  #'  SP = "AAA",
  #'  BondLab = "Aaa",
  #'  Frequency = 12,
  #'  BondBasis = "30360",
  #'  GWac = 4.75,
  #'  OrigLoanBal = 275000,
  #'  OrigLTV = 80,
  #'  AmortizationType = "fixed",
  #'  AmortizationTerm = 30,     
  #'  Index = "None",
  #'  Margin = 0,
  #'  FirstPmtDate = "01-01-2013",
  #'  FinalPmtDate = "01-01-2043",
  #'  Servicing = 0.25,
  #'  PMI = 0.25,
  #'  GFeePremium = 0.25,
  #'  InitialInterest = FALSE,
  #'  InterestOnlyPeriod = 0,
  #'  FirstPrinPaymentDate = "1-01-2013",
  #'  BalloonPmt = FALSE,
  #'  BalloonDate = "00-00-0000",
  #'  MBSFactor = 1,
  #'  OriginalBal = 1000000,
  #'  CurrentBal = 1000000,
  #'  Model = "FH30.Generic",
  #'  Burnout = 25,
  #'  SATO = 0.75)} 
  #' @export MakeMBSDetails
  MakeMBSDetails <- function(
  Cusip = "character",
  ID = "character",
  BondType = "character",
  Issuer = "character",
  Underwriter = "character",
  Sector ="character",
  Coupon = numeric(),
  IssueDate = "character",
  DatedDate = "character",
  Maturity = "character",
  LastPmtDate = "character",
  NextPmtDate = "character",
  Term = numeric(),
  WALA = numeric(),
  WAM = numeric(),
  PaymentDelay = numeric(),
  Moody = "character",
  SP = "character",
  BondLab  = "character",
  Frequency = numeric(),
  BondBasis = "character",
  GWac = numeric(),
  OrigLoanBal = numeric(),
  OrigLTV = numeric(),
  AmortizationType = "character",
  AmortizationTerm = numeric(),
  Index = "character",
  Margin = numeric(),
  FirstPmtDate = "character",
  FinalPmtDate = "character",
  Servicing = numeric(),
  PMI = numeric(),
  GFeePremium = numeric(),
  InitialInterest = "logical",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "logical",
  BalloonDate = "character",
  MBSFactor = numeric(),
  OriginalBal = numeric(),
  CurrentBal = numeric(),
  Model = "character",
  Burnout = numeric(),
  SATO = numeric())
{
  
  temp <- MBSDetails(
    Cusip = Cusip,
    ID = ID,
    BondType = BondType,
    Issuer = Issuer,
    Underwriter = Underwriter,
    Sector = Sector,
    Coupon = Coupon,
    IssueDate = IssueDate,
    DatedDate = DatedDate,
    Maturity = Maturity,
    LastPmtDate = LastPmtDate,
    NextPmtDate = NextPmtDate,
    Term = Term,
    WALA = WALA,
    WAM = WAM,
    PaymentDelay = PaymentDelay,
    Moody = Moody,
    SP = SP,
    BondLab  = BondLab,
    Frequency = Frequency,
    BondBasis = BondBasis,
    GWac = GWac,
    OrigLoanBal = OrigLoanBal,
    OrigLTV = OrigLTV,
    AmortizationType = AmortizationType,
    AmortizationTerm = AmortizationTerm,
    Index = Index,
    Margin = Margin,
    FirstPmtDate = FirstPmtDate,
    FinalPmtDate = FinalPmtDate,
    Servicing = Servicing,
    PMI = PMI,
    GFeePremium = GFeePremium,
    InitialInterest = InitialInterest,
    InterestOnlyPeriod = InterestOnlyPeriod,
    FirstPrinPaymentDate = FirstPrinPaymentDate,
    BalloonPmt = BalloonPmt,
    BalloonDate = BalloonDate,
    MBSFactor = MBSFactor,
    OriginalBal = OriginalBal,
    CurrentBal = CurrentBal,
    Model = Model,
    Burnout = Burnout,
    SATO = SATO)
  
    SaveMBS(filename = temp)}
