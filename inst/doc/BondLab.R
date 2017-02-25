## ---- setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(cache=FALSE)
require(ggplot2)
require(reshape2)
require(scales)
require(termstrc)
require(BondLab)

## ---- echo = FALSE, fig.align='center', fig.height= 5, fig.width= 7------
 Valuation <- function(){
    cbbPalette <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', 
                    '#D55E00', '#CC79A7', '#000000','#E69F00')

  SpotCurve <- spr_ns(c(4.26591, -3.54742, 2.46095, 7.49599), m = 1:30)
  Maturity <- as.numeric(seq(1:30))
  Coupon <- SpotCurve - .25

  Data <- data.frame(Maturity = Maturity, 
                     CouponCurve = Coupon, 
                     SpotRateCurve = SpotCurve, 
                     ZVSpread = SpotCurve +.20, 
                     YTM = 3.25)
  Data <- melt(Data, id = 'Maturity')
  
  framework <-
  ggplot(Data, aes(x= Maturity, y = value/100, colour = variable))+
  geom_line(aes(linetype = variable), size = 1)+
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted', 'twodash')) +
  scale_y_continuous(breaks = seq(0, .04, .005), labels = percent_format())+
  geom_vline(xintercept = 10)+
  ylab('Yield') +
  xlab('Maturity (years)') +
  scale_colour_manual(values = cbbPalette) +
  theme_minimal()+  
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
        panel.grid.major = element_line(size = .25, color = 'grey'),
        legend.title=element_blank(),
        legend.position=c(.80,.25), 
        legend.background = element_rect(fill = 'transparent', 
                                         colour = 'transparent')
        )+
  # Add annotations describing the valuation framework for mortgage and asset 
  # backed securities
  # 
  # Add annotation labeling: the Nominal Spread to the curve
    annotate('pointrange', x = 9, y = 2.95/100, ymin = 2.65/100, 
             ymax = 3.25/100, colour = 'black', size = .5)+
    geom_segment(aes(x= 1, y = 2.8/100, xend = 8.75, yend = 2.8/100), 
               arrow = arrow(length = unit(0.2, 'cm')), colour = 'black') +
    annotate('text', x=4.35, y = 2.9/100, label = c('Nominal Spread')) +
  
  # Add annotation labeling: the ZV spread
    annotate('pointrange', x= 19, y = 3.78/100, ymin = 3.67/100, ymax = 3.89/100, 
           colour = 'black', size = .5) +
    geom_segment(aes(x= 12.5, y = 3.75/100, xend = 18.5, yend = 3.75/100), 
               arrow = arrow(length = unit(0.2, 'cm')), colour = 'black') +
    annotate('text', x=14.75, y = 3.85/100, label = c('ZV-Spread')) +

  # Label Cheap Cash Flows region on the graph
  annotate('text', x=4.35, y = 4.25/100, 
           label = c('Cheap Cash Flows\n PV Spot > PV YTM'), size = 4) +
  # Label Rich Cash Flows region on the graph
  annotate('text', x=16, y = 4.25/100, 
           label = c('Rich Cash Flows\n PV Spot < PV YTM'), size = 4)
  plot(framework)
 }
Valuation()

## ---- priceclass, echo=TRUE----------------------------------------------
      price <- "103-16"
      tradedate <- "07-11-2016"
      settlementdate <- "08-18-2016"
    # note PriceTypes class is used to convert price from string to
    # numeric decimal equivilant
    Price <- PriceTypes(price = price)

## ---- termstructure, echo = TRUE-----------------------------------------
   rates.data <- Rates(trade.date = "07-11-2016")
   # note use invisible(capture.output()) to supress messages
   invisible(capture.output(
     TermStructure <- TermStructure(rates.data = rates.data, method = "ns")))

## ---- bonddata, echo=TRUE------------------------------------------------
      bond.id <- MBS(MBS.id = "FHQ41072")

## ---- prepayment, echo=TRUE----------------------------------------------
    MortgageRate <- MtgRate()
    ModelTune <- ModelTune(bond.id = bond.id)
    #invoke the prepayment model and assign it to object
    Prepayment <- PrepaymentModel(bond.id = bond.id,
                                  TermStructure = TermStructure,
                                  MortgageRate = MortgageRate,
                                  ModelTune = ModelTune,
                                  PrepaymentAssumption = "MODEL")

## ---- cashflow, echo=TRUE------------------------------------------------
PassThrough <-
  MortgageCashFlow(bond.id = bond.id,
                   original.bal = OriginalBal(bond.id),
                   settlement.date = settlementdate,
                   # note: here price is passed as decimal eqivalent string
                   # internally this function also uses PriceTypes to convert
                   # price to a numeric decimal basis
                   price = PriceDecimalString(Price),
                   PrepaymentAssumption = Prepayment)

## ---- spreads, echo=TRUE-------------------------------------------------
# note: used getter methods on the classes to calculate proceeds
  proceeds = OriginalBal(bond.id) *MBSFactor(bond.id) * PriceBasis(Price)
# The class curve spreads calculates curve spreads for reporting
# or in this case to pass zero volatility spread to the total return function  
  CurveSpreads <- CurveSpreads(rates.data = rates.data,
                               TermStructure = TermStructure,
                               CashFlow = PassThrough,
                               proceeds = proceeds)

## ---- total return, echo=TRUE--------------------------------------------

  invisible(capture.output(
    NoChangeScenario <- MortgageScenario(
      bond.id = bond.id,
      settlement.date = settlementdate,
      #note: rates data is passed rather than term structure object
      #this is because the scenario operates on either the spot curve
      #taken from term structure object of on coupon curve (rates data)
      rates.data = rates.data,
      price = PriceDecimalString(Price),
      original.bal = OriginalBal(bond.id),
      scenario = "NCs",
      horizon.months = 12,
      method = "ns",
      prepayment = "MODEL",
      horizon.spot.spread = ZeroVolSpread(CurveSpreads))))

## ---- return, echo=TRUE--------------------------------------------------
HorizonReturn(NoChangeScenario)
ZeroVolSpread(NoChangeScenario)
SpreadToCurve(NoChangeScenario)
SpreadToBenchmark(NoChangeScenario)
BenchMark(NoChangeScenario)
WAL(NoChangeScenario)
EffDuration(NoChangeScenario)
barplot(KeyRateDuration(NoChangeScenario))

## ---- MyPassThrough, echo= TRUE------------------------------------------
MyPassThrough <- function(bond.id = "character",
                         trade.date = "character",
                         settlement.date = "character",
                         prepayment = "character",
                         ...,
                         price = NULL,
                         spread = NULL,
                         CPR = numeric()){
  
  Price <- PriceTypes(Price = price)
  bond.id <- MBS(MBS.id = "FHQ41072")
  rates.data <- Rates(trade.date = trade.date)
  MortgageRate <- MtgRate()
  ModelTune <- ModelTune(bond.id = bond.id) 
  
  invisible(capture.output(
     TermStructure <- TermStructure(rates.data = rates.data, method = "ns")))
  
    #invoke the prepayment model and assign it to object
    Prepayment <- PrepaymentModel(bond.id = bond.id,
                                  TermStructure = TermStructure,
                                  MortgageRate = MortgageRate,
                                  ModelTune = ModelTune,
                                  PrepaymentAssumption = prepayment,
                                  CPR = CPR)}

