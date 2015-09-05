# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#======================  Set and create class MBS Details ==============================
# Do I need a generic for this?
# ---------------------       Inititialize MBSDetails     ------------------------------
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
                   Gfee = "numeric",
                   InitialInterest = "logical",
                   InterestOnlyPeriod = "numeric",
                   FirstPrinPaymentDate = "character",
                   BalloonPmt = "logical",
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
            .Object@OrigLoanBal = OrigLoanBal
            .Object@OrigLTV = OrigLTV
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
  Gfee = numeric(),
  InitialInterest = "logical",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "logical",
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


#' A constructor function to create a mortgage pass through security
#' 
#' This is a standard generic function used to construct a MBS pass through security
#' @param Cusip A character the Pass Through MBS cusip.
#' @param ID A character string the pool number. 
#' @param BondType A character string the type of Bond MBS, etc.
#' @param Sector A charcter string description of the Sector Mortgage, Utility, Government.
#' @param Coupon A numeric value the Bond Coupon.
#' @param IssueDate A character string the issue date of the security.
#' @param DatedDate A character sting The date following the issue when interest begins to accure.
#' @param Maturity A character sting the final payment date to the investor
#' in the case MBS the final payment data assuming 0 CPR.
#' @param LastPmtDate A character string the date the last payment scheduled payment to the investor.
#' @param NextPmtDate A character string the date of the next scheduled payment to the investor.
#' @param PaymentDelay A numeric value in the case of MBS the delay of the payment 
#' from the trust to the investor
#' @param Moody A character string Moody's assigned credit rating
#' @param SP A character string SP's assigned credit rating
#' @param BondLab A character string BondLab's or the user's assigned credit rating
#' @param Frequency A numeric value string the frequency of payments made to the investor
#' @param BondBasis A character string the basis on which interest is calculated
#' @param GWac A numeric value the borrower's note rate
#' @param OrigLoanBal A numeric value the original balance of the loan
#' @param OrigLTV A numeric value the borrower's original loan to value ratio
#' @param AmortizationType A character sting the type of the loan 'fixed' or 'arm'.
#' These values are used by the prepayment model to drive the mortgage rate either fixed
#' or adjustable mortgage rate
#' @param AmortizationTerm A numeric value the term of the loan in years
#' @param Index A character string if the amortization type is adjustable the Index 
#' to which the note rate references
#' @param Margin A numeric value the spread over the index used to determine 
#' the borrower's note rate
#' @param FirstPmtDate A character string the date of the first payment 
#' of the borrower's note.  In the case of an the first payment made to the bondholder
#' @param FinalPmtDate A character string the date of thee final payment 
#' of the borrower's note.  In the case of an MBS the final payment made to the bondholder
#' @param Servicing A numeric value the servicing spread from the Gross WAC to the servicer
#' of hte mortgage pool's underlying loans.
#' @param PMI A numeric value the primary mortage insurance paid by the borrower to the 
#' PMI provider
#' @param Gfee A numeric value the guarantee fee taken from the borrower's note rate to
#' guarantee timely payment of principal and interest.  Applicable in the case of Fannie
#' Mae, Freddie Mac, or Ginnie Mae pools.
#' @param InitialInterest A logical indicating the note carries an interest only period
#' @param InterestOnlyPeriod A character string indicating the note's interest only period
#' @param FirstPrinPaymentDate A character string indicating the first principal payment date
#' due of the mortgage.
#' @param BalloonPmt A logical indicating the mortgage carries a balloon pmt.
#' @param BalloonDate A character string the balloon payment date.
#' @param MBSFactor A numeric value the current factor of the MBS.
#' @param Model A character string the prepayment model to use.
#' @param Burnout A numeric model the value of the borrower burnout.
#' @param SATO A numeric value the borrrowers Spread AT Origination over the prime lending rate.
#' @examples 
#' \dontrun{
#'  MakeMBSDetails( 
#'  Cusip = "23456789",
#'  ID = "bondlabMBS4",
#'  BondType = "MBS",
#'  Sector = "MBS",
#'  Coupon = 4.0,
#'  IssueDate = "01-01-2013",
#'  DatedDate = "01-01-2013",
#'  Maturity = "01-01-2043",
#'  LastPmtDate = "01-01-2013",
#'  NextPmtDate = "02-01-2013",
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
#'  Gfee = 0.25,
#'  InitialInterest = FALSE,
#'  InterestOnlyPeriod = 0,
#'  FirstPrinPaymentDate = "1-01-2013",
#'  BalloonPmt = FALSE,
#'  BalloonDate = "00-00-0000",
#'  MBSFactor = 1,
#'  Model = "FH30.Generic",
#'  Burnout = 25,
#'  SATO = 0.75)} 
#' @export
  
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
  Gfee = numeric(),
  InitialInterest = "logical",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "logical",
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
  
   connMBSDetails <- gzfile(description = paste(system.file(package = "BondLab"),
                                       "/BondData/",temp@ID,".rds", sep = ""))
   saveRDS(temp, connMBSDetails)
   close(connMBSDetails)
 #  SaveMBS(filename = "temp")
}

setGeneric("MakeMBSDetails", function(
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
  Gfee = numeric(),
  InitialInterest = "logical",
  InterestOnlyPeriod = numeric(),
  FirstPrinPaymentDate = "character",
  BalloonPmt = "logical",
  BalloonDate = "character",
  MBSFactor = numeric(),
  Model = "character",
  Burnout = numeric(),
  SATO = numeric())
{standardGeneric("MakeMBSDetails")})