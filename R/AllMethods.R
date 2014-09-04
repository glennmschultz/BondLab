# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 



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


# Initialize TermStructure
setMethod("initialize",
          signature("TermStructure"),
          function(.Object,...,
                   tradedate = "character",
                   period = "numeric",
                   date = "character",
                   spotrate = "numeric",
                   forwardrate = "numeric",
                   TwoYearFwd = "numeric",
                   TenYearFwd = "numeric")
          {
            .Object@tradedate = tradedate
            .Object@period = period
            .Object@date = date
            .Object@spotrate = spotrate
            .Object@forwardrate = forwardrate
            .Object@TwoYearFwd = TwoYearFwd
            .Object@TenYearFwd = TenYearFwd
            
            return(.Object)
            callNextMethod(.Object,...)
          })

# Initialize MortgageCashFlows
setMethod("initialize",
          signature("MortgageCashFlows"),
          function(.Object,       
                   Price = numeric(),
                   Accrued = numeric(),
                   YieldToMaturity = numeric(),
                   WAL = numeric(),
                   ModDuration = numeric(),
                   Convexity = numeric(),
                   Period = numeric(),
                   PmtDate = "character",
                   TimePeriod = numeric(),
                   BeginningBal = numeric(),
                   MonthlyPmt = numeric(),
                   MonthlyInterest = numeric(),
                   PassThroughInterest = numeric(),
                   ScheduledPrin = numeric(),
                   PrepaidPrin = numeric(),
                   EndingBal = numeric(),
                   ServicingIncome = numeric(),
                   PMIPremium = numeric(),
                   GFeePremium = numeric(),  
                   TotalCashFlow = numeric()
          ){
            
            .Object@Price = Price
            .Object@Accrued = Accrued
            .Object@YieldToMaturity = YieldToMaturity
            .Object@WAL = WAL
            .Object@ModDuration = ModDuration
            .Object@Convexity = Convexity
            .Object@Period = Period
            .Object@PmtDate = PmtDate
            .Object@TimePeriod = TimePeriod
            .Object@BeginningBal = BeginningBal
            .Object@MonthlyPmt = MonthlyPmt
            .Object@MonthlyInterest = MonthlyInterest
            .Object@PassThroughInterest = PassThroughInterest
            .Object@ScheduledPrin = ScheduledPrin
            .Object@PrepaidPrin = PrepaidPrin
            .Object@EndingBal = EndingBal
            .Object@ServicingIncome = ServicingIncome
            .Object@PMIPremium = PMIPremium
            .Object@GFeePremium = GFeePremium  
            .Object@TotalCashFlow = TotalCashFlow
            
            return(.Object)
            callNextMethod(.Object,...) 
          })


# This section begins the Bond Methods
setMethod("show",
          signature(object = "BondCashFlows"),
          function (object) 
          {      
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            plotdata = as.data.frame(cbind(object@Period, object@TotalCashFlow))
            colnames(plotdata) <- c("Period", "CashFlow")
            
            plot <- ggplot(plotdata, aes(x= Period, y = CashFlow)) +
              geom_bar(stat = "identity", fill = "Grey") +
              theme_minimal() + 
              labs(fill = "") +
              ylab("Bond Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
            
            print(plot)
            
          }
)

setMethod("show", 
          signature(object = "BondAnalytics"),
          function(object)
          {
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Effective Duration"); print(unname(object@EffDuration))
            cat("Effective Convexity"); print(unname(object@EffConvexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            plotdata1 = as.data.frame(cbind(object@Period, object@TotalCashFlow))
            colnames(plotdata1) <- c("Period", "CashFlow")
            
            plot1 <- ggplot(plotdata1, aes(x= Period, y = CashFlow)) +
              geom_bar(stat = "identity", fill = "Grey") +
              theme_minimal() + 
              labs(fill = "") +
              ylab("Bond Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
            
            plotdata2 <- as.data.frame(cbind(object@KeyRateTenor, object@KeyRateDuration))
            colnames(plotdata2) <- c("KRTenor", "KRDuration")
            
            plot2 <- ggplot(plotdata2, aes(x = as.factor(KRTenor), y = KRDuration)) +
              geom_bar(stat = "identity", fill = "Grey") +
              theme_minimal() +
              labs(fill = "") +
              ylab("Key Rate Duration") +
              xlab("Key Rate Tenor") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
            
            multiplot(plot1, plot2, cols = 1)
            
          }
)





# ===== This section begins the MBS Methods


setMethod("show",
          signature(object = "MortgageCashFlows"),
          function (object) 
          {      
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            
            plotdata = as.data.frame(cbind(object@Period, object@ScheduledPrin, object@PrepaidPrin, 
                                           object@PassThroughInterest, object@ServicingIncome, object@PMIPremium, object@GFeePremium))
            colnames(plotdata) <- c("Period", "Scheduled Prin", "Prepaid Prin", "PT Interest", "Servicing", "PMI", "GFee")
            plotdata = melt(plotdata, id = "Period")
            
            plot <- ggplot(plotdata, aes(x= Period, y = value, fill = variable)) +
              geom_area() +
              theme_minimal()+
              scale_fill_brewer(palette = "Greys") +
              labs(fill = "") +
              ylab("Pool Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))+
              theme(legend.background = element_rect(fill = "white"))
            
            print(plot)
          }
)



setMethod("show", 
          signature(object = "PassThroughAnalytics"),
          function(object)
          {
            cat("Bond Description", "\n")
            cat("BondId:"); print(object@ID)
            cat("Cusip:"); print(object@Cusip)
            cat("Coupon:"); print(object@Coupon)
            cat("Frequency:"); print(object@Frequency)
            cat("Basis:"); print(object@BondBasis)
            cat("Issue Date:"); print(object@IssueDate)
            cat("Last Payment Date:"); print(object@LastPmtDate)
            cat("Next Payment Date:"); print(object@NextPmtDate)
            cat("Maturity Date:"); print(object@Maturity)
            cat("Bond Valuation:", "\n")
            cat("Price:"); print(object@Price)
            cat("Accrued:"); print(object@Accrued)
            cat("Yield to Maturity:"); print(object@YieldToMaturity)
            cat("Risk Metrics:", "\n")
            cat("Weighted Average Life:"); print(object@WAL)
            cat("Modified Duration:"); print(unname(object@ModDuration))
            cat("Convexity:"); print(unname(object@Convexity))
            cat("Effective Duration"); print(unname(object@EffDuration))
            cat("Effective Convexity"); print(unname(object@EffConvexity))
            cat("Sector Detail:", "\n")
            cat("Bond Type:"); print(object@BondType)
            cat("Sector:"); print(object@Sector)
            cat("Moodys:"); print(object@Moody)
            cat("S&P:"); print(object@SP)
            cat("BondLab Rating:");print(object@BondLab)
            
            plotdata1 = as.data.frame(cbind(object@Period, object@TotalCashFlow))
            colnames(plotdata1) <- c("Period", "CashFlow")
            
            plot1 <- ggplot(plotdata1, aes(x= Period, y = CashFlow)) +
              geom_bar(stat = "identity", width = 1, fill = "Grey") +
              theme_minimal() + 
              labs(fill = "") +
              ylab("Mtg. Cash Flow") +
              xlab("Period") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
            
            plotdata2 = as.data.frame(cbind(object@KeyRateTenor, object@KeyRateDuration))
            colnames(plotdata2) <- c("KRTenor", "KRDuration")
            
            plot2 <- ggplot(plotdata2, aes(x = as.factor(KRTenor), y = KRDuration)) +
              geom_bar(stat = "identity", fill = "Grey") +
              theme_minimal() +
              labs(fill = "") +
              ylab("Key Rate Duration") +
              xlab("Key Rate Tenor") +
              theme(axis.title.y=element_text(angle = 90, size = 20)) +
              theme(axis.text.y = element_text(angle = 90, size = 15)) +
              theme(axis.title.x=element_text(angle = 0, size = 20)) +
              theme(axis.text.x = element_text(angle = 0, size = 15)) +
              theme(legend.position = c(.82,.73))
            
            multiplot(plot1, plot2, cols = 1)
            
          }
)

setMethod("show",
          signature(object = "DollarRoll"),
          function(object){
            cat("Dollar Roll Analysis", "\n")
            cat("Settlement Date"); print(object@SettlementDate)
            cat("Settlement Price"); print(object@Price)
            cat("Drop 32nds"); print(object@Drop)
            cat("Forward Settlement Date"); print(object@FwdSettlementDate)
            cat("Forward Price"); print(object@FwdPrice)
            cat("Beginning Market Value"); print(object@PrincipalProceeds)
            cat("Accrued Interest"); print(object@Accrued)
            cat("Roll Proceeds"); print(object@TotalProceeds)
            cat("Reinvestment Proceeds"); print(object@ReinvestmentIncome)
            cat("Future Value"); print(object@FutureValueRoll)
            cat("Dollar Advantage"); print(object@Advantage)
            #cat("Basis Points (Annualized"); print(object@BasisPoints)
            cat("Breakeven Drop"); print(object@DropImpliedValue)
            cat("Hold or Roll"); print(object@HoldorRoll)
            cat("Scheduled Principal"); print(object@ScheduledPrin)
            cat("Prepaid Principal"); print(object@PrepaidPrin)
            cat("Pass Through Interest"); print(object@PassThroughInterest)
            cat("Remaining Principal"); print(object@RemainingBalance)
            cat("Proceeds"); print(object@FuturePrincipalProceeds)
            cat("Hold Accrued"); print(object@FwdAccrued)
            cat("Future Value"); print(object@FutureValuePrinCarry)
          }
)






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


  
