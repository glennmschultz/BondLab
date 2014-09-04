

# MBS generic functions 

  setGeneric("MBSDetails", function(Cusip = "character", ID = "character", BondType = "character", Sector ="character", Coupon = numeric(),
                                  IssueDate = "character", DatedDate = "character", Maturity = "character", LastPmtDate = "character", NextPmtDate = "character",
                                  PaymentDelay = numeric(), Moody = "character", SP = "character", BondLab  = "character", Frequency = numeric(), 
                                  BondBasis = "character", GWac = numeric(), AmortizationType = "character", AmortizationTerm = numeric(), Index = "character",
                                  Margin = numeric(), FirstPmtDate = "character", FinalPmtDate = "character", Servicing = numeric(), PMI = numeric(),
                                  Gfee = numeric(), InitialInterest = "character", InterestOnlyPeriod = numeric(), FirstPrinPaymentDate = "character",
                                  BalloonPmt = "character", BalloonDate = "character", MBSFactor = numeric(), Model = "character", Burnout = numeric(), SATO = numeric()) 
                                  {standardGeneric("MBSDetails")})

  setGeneric("TermStructure",
           function(rates.data = "character", method = "character")
           {standardGeneric("TermStructure")})
  

  setGeneric("MortgageCashFlows", function(bond.id = "character", original.bal = numeric(), settlement.date = "character", 
    price = numeric(), PrepaymentAssumption = "character")
    {standardGeneric("MortgageCashFlows")})