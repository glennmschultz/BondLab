ratedata <- readRDS("~/BondLab/RatesData/2013-01-10.rds")
oas.test <- MortgageOAS(bond.id = "bondlabMBS4", trade.date = "01-10-2013", original.bal = 100000, price = 100)

connS2 <- gzfile(description = paste("~/BondLab/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
rates.data <- readRDS(connS2)

rates.data = readRDS("~/BondLab/RatesData/2013-01-10.rds")


TermStructure(rates.data = readRDS("~/BondLab/RatesData/2013-01-10.rds"), method = "ns")

test <- readRDS("~/BondLab/RatesData/2013-01-10.rds")



