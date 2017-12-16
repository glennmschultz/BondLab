## ---- setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(cache=FALSE)
require(termstrc)
require(BondLab)
require(sqldf)
require(RSQLite)
APIKEY <- "31df8645e635484db99e5d1133ce9245"

## ---- sqlquery, echo=TRUE------------------------------------------------
  MBSData <- dbConnect(SQLite(), dbname=paste0(system.file(package = "BondLab"), "/BondData/MBSData"))
  dbGetQuery(MBSData,
             'Select Cusip
             ,Coupon
             ,AmortizationTerm
             ,price 
             from MBS')

## ---- priceclass, echo=TRUE----------------------------------------------
      cusip = "31283HY43"
      price <- dbGetQuery(MBSData, 'Select price from MBS where cusip = "31283HY43"')
      tradedate <- '05-19-2017'
      settlementdate <- '06-15-2017'
    # note PriceTypes class is used to convert price from string to
    # numeric decimal equivilant
    Price <- PriceTypes(price = as.character(price))

## ---- bonddata, echo=TRUE------------------------------------------------
    bond.id <- MBS(cusip = cusip)

