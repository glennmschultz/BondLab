
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
  
  #' @include MBSDetails.R BondDetails.R
  NULL
  
  #'@title CreateCashFlowMatrix
  #'@family Interest Rate Models
  #'@description An dataframe containing bond cash flow.  The first three rows
  #'include the price, accured interest, and yield to maturity, and modified 
  #'duration. CashFlowMatrix[5:nrow,] matches the maturity matrix for yield curve
  #'fitting
  #'should match the number of rows of the maturity matrix
  #'@param trade.date The trade date 
  #'@param bonddata A dataframe of bond data.  The complete dataframe must
  #'consist of the following names columns: cusip, interestrate, issuedate,
  #'dateddate, maturitydate, type, close.
  #'@import bizdays
  #'@importFrom lubridate leap_year days
  #'@export CreateCashFlowMatrix
  CreateCashFlowMatrix <- function(trade.date, bonddata){
    bonddata <- bonddata[order(as.Date(bonddata$maturitydate, format = '%Y-%m-%d')),]
    
    cash_flow_matrix <- as.data.frame(matrix(data = 0,
                                             nrow = 60,
                                             ncol = nrow(bonddata)))
    price_matrix <- as.data.frame(matrix(data = 0,
                                         nrow = 4 ,
                                         ncol = nrow(bonddata)))
    cal <- 'actual'
    leap_day <- function(pmt.date){
      for(date in seq_along(pmt.date)){
        if(leap_year(pmt.date)[date] == TRUE & 
           month(pmt.date)[date] == 2 & day(pmt.date)[date] == 28){
          pmt.date[date] = pmt.date[date] %m+% days(1)} else {next}
      }
      return(pmt.date)
    }
    
    for(bonds in 1:nrow(bonddata)){
      IssueDate = as.character(as.Date(bonddata[bonds, 'issuedate']), format = '%m-%d-%Y') 
      DatedDate = as.character(as.Date(bonddata[bonds, 'dateddate']), format = '%m-%d-%Y')
      MaturityDate = as.character(as.Date(bonddata[bonds, 'maturitydate']), format = '%m-%d-%Y')
      Coupon = bonddata[bonds, 'interestrate']
      SettlementDate = as.character(add.bizdays(as.character(trade.date), 2, cal), 
                                    format = '%m-%d-%Y')
      
      PmtDates = LastandNextPmtDate(issue.date = IssueDate,
                                    dated.date = DatedDate,
                                    maturity.date = MaturityDate,
                                    settlement.date = SettlementDate,
                                    bond.basis = 'Actual365',
                                    frequency = 2)
      PmtDates = leap_day(PmtDates)
      
      ID = gsub("[[:punct:]]", "", paste0('UST',Coupon, 
                                          format(as.Date(MaturityDate, 
                                                         format = '%m-%d-%Y'), 
                                                 format ='%m%-%Y')))
      
      Bond <- BondDetails(
        Cusip = bonddata[bonds,'cusip'],
        ID = ID,
        BondType = bonddata[bonds,'type'],
        Sector = 'UST',
        Issuer = 'US Govt',
        Underwriter = 'US Govt',
        OfferAmount = 100,
        Coupon = Coupon,
        IssueDate = IssueDate,
        DatedDate = DatedDate,
        Maturity = MaturityDate,
        LastPmtDate = as.character(as.Date(PmtDates[1]), format = '%m-%d-%Y'),
        NextPmtDate = as.character(as.Date(PmtDates[2]), format = '%m-%d-%Y'),
        Moody = 'Aaa',
        SP = 'AAA',
        BondLab = 'AAA',
        Frequency = 2,
        BondBasis = 'Actual365',
        Callable = FALSE,
        Putable = FALSE,
        SinkingFund = FALSE
      )
      
      Price <- PriceTypes(as.character(bonddata[bonds,'close']))
      
      CashFlow <- tryCatch(BondCashFlows(bond.id = Bond, 
                                         principal = OfferAmount(Bond), 
                                         settlement.date = SettlementDate,
                                         price = PriceDecimalString(Price)),
                           error = function(e) return (NULL))
      numcashflow <- length(TotalCashFlow(CashFlow))
      price_matrix[1,bonds] <- PriceDecimal(Price)
      price_matrix[2,bonds] <- Accrued(CashFlow)
      price_matrix[3,bonds] <- YieldToMaturity(CashFlow)
      price_matrix[4,bonds] <- ModDuration(CashFlow)
      cash_flow_matrix[1:numcashflow,bonds] <- TotalCashFlow(CashFlow)
      #cash_flow_matrix[1:numcashflow,bonds] <- TimePeriod(CashFlow)
    }
    cash_flow_matrix <- rbind(price_matrix, cash_flow_matrix)
    colnames(cash_flow_matrix) <- c(bonddata$cusip)
    return(cash_flow_matrix)
  }
  
  #'@title CreateMaturitiesMatrix
  #'@family Interest Rate Models
  #'@description An dataframe containing maturity for discounting
  #'@param trade.date The trade date 
  #'@param bonddata A dataframe of bond data.  The complete dataframe must
  #'consist of the following names columns: cusip, interestrate, issuedate,
  #'dateddate, maturitydate, type, close
  #'@import bizdays
  #'@importFrom lubridate leap_year days
  #'@export CreateMaturitiesMatrix
  CreateMaturitiesMatrix <- function(trade.date, bonddata){
    bonddata <- bonddata[order(as.Date(bonddata$maturitydate, format = '%Y-%m-%d')),]
    
    maturity_matrix <- as.data.frame(matrix(data = 0,
                                             nrow = 60,
                                             ncol = nrow(bonddata)))

    cal <- 'actual'
    leap_day <- function(pmt.date){
      for(date in seq_along(pmt.date)){
        if(leap_year(pmt.date)[date] == TRUE & 
           month(pmt.date)[date] == 2 & day(pmt.date)[date] == 28){
          pmt.date[date] = pmt.date[date] %m+% days(1)} else {next}
      }
      return(pmt.date)
    }
    
    for(bonds in 1:nrow(bonddata)){
      IssueDate = as.character(as.Date(bonddata[bonds, 'issuedate']), format = '%m-%d-%Y') 
      DatedDate = as.character(as.Date(bonddata[bonds, 'dateddate']), format = '%m-%d-%Y')
      MaturityDate = as.character(as.Date(bonddata[bonds, 'maturitydate']), format = '%m-%d-%Y')
      Coupon = bonddata[bonds, 'interestrate']
      SettlementDate = as.character(add.bizdays(as.character(trade.date), 2, cal), 
                                    format = '%m-%d-%Y')
      
      PmtDates = LastandNextPmtDate(issue.date = IssueDate,
                                    dated.date = DatedDate,
                                    maturity.date = MaturityDate,
                                    settlement.date = SettlementDate,
                                    bond.basis = 'Actual365',
                                    frequency = 2)
      PmtDates = leap_day(PmtDates)
      
      ID = gsub("[[:punct:]]", "", paste0('UST',Coupon, 
                                          format(as.Date(MaturityDate, 
                                                         format = '%m-%d-%Y'), 
                                                 format ='%m%-%Y')))
      
      Bond <- BondDetails(
        Cusip = bonddata[bonds,'cusip'],
        ID = ID,
        BondType = bonddata[bonds,'type'],
        Sector = 'UST',
        Issuer = 'US Govt',
        Underwriter = 'US Govt',
        OfferAmount = 100,
        Coupon = Coupon,
        IssueDate = IssueDate,
        DatedDate = DatedDate,
        Maturity = MaturityDate,
        LastPmtDate = as.character(as.Date(PmtDates[1]), format = '%m-%d-%Y'),
        NextPmtDate = as.character(as.Date(PmtDates[2]), format = '%m-%d-%Y'),
        Moody = 'Aaa',
        SP = 'AAA',
        BondLab = 'AAA',
        Frequency = 2,
        BondBasis = 'Actual365',
        Callable = FALSE,
        Putable = FALSE,
        SinkingFund = FALSE
      )
      
      Price <- PriceTypes(as.character(bonddata[bonds,'close']))
      
      CashFlow <- tryCatch(BondCashFlows(bond.id = Bond, 
                                         principal = OfferAmount(Bond), 
                                         settlement.date = SettlementDate,
                                         price = PriceDecimalString(Price)),
                           error = function(e) return (NULL))
      numcashflow <- length(TotalCashFlow(CashFlow))
      maturity_matrix[1:numcashflow,bonds] <- TimePeriod(CashFlow)
    }
    colnames(maturity_matrix) <- c(bonddata$cusip)
    return(maturity_matrix)
  }
  
  