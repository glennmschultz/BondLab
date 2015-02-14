# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics" 



  #---------- connect to the rates data folder
#' @export Rates
   Rates <- function(trade.date = "character")
                  {
                  Rates.Conn <-gzfile(description = paste("~/BondLab/RatesData/", 
                                        as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
                  Rates <- readRDS(Rates.Conn)
                  return(Rates)
                  }
   
   #-------- connect to mortgage rate function classes
   MtgRate <- function()
                  {
                   MtgRate.Conn <- gzfile("~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
                   MtgRate <- readRDS(MtgRate.Conn)
                   }
   
   #------- connect to updated LTV function class
   ULTV <- function()
                  {
                  ULTV.Conn <- gzfile("~/BondLab/PrepaymentModel/UpdatedLTV.rds", open = "rb")
                  ULTV <- readRDS(ULTV.Conn)
                  }
   
  #----------- connect to prepayment model tune data folder
   ModelTune <- function(bond.id = "character")
                 {
                 ModelTune.Conn <- gzfile(description = paste("~/BondLab/PrepaymentModel/", 
                                                              bond.id@Model, ".rds", sep =""), open = "rb")        
                 ModelTune <- readRDS(ModelTune.Conn)    
                 }

  #----------- connect to MBS cusip data folder
    MBS <- function(MBS.id = "character")
                  {
                  MBS.Conn <- gzfile(description = paste("~/BondLab/BondData/",
                                                         MBS.id, ".rds", sep = ""), open = "rb")
                  MBS <- readRDS(MBS.Conn)
                  return(MBS)
                  }
     
  #----------- connect to REMIC deal file
    REMICDeal <- function(remic.deal = "character") 
                 {
                  REMIC.Conn <- gzfile(description = paste("~/BondLab/REMICData/",
                                                          remic.deal, ".rds", sep = ""), open = "rb")
                  
                  REMICDeal <- readRDS(REMIC.Conn)
                  return(REMICDeal)
                  }
   
   #---------- connect to waterfall data
   REMICWaterFall <- function(deal.name = "character")
                  {
                   paste("~/BondLab/WaterFall/",deal.name, sep = "")
                  }
   
   #---------- connect to retrieve bond schedule data 
    REMICSchedules <- function(REMIC.Tranche = "character")
                  {
                  Sched.Conn <- gzfile(paste("~/BondLab/Schedules/",
                                as.character(REMIC.Tranche@DealName), "_Group_", as.character(REMIC.Tranche@Group ), "_Sch", ".rds",
                                sep = ""), open = "rb")
                    
                    REMICSchedules <- readRDS(Sched.Conn)
                    #return(REMICSchedules) No need to return as this is not saved via a function wrapper but used
                    # in the waterfall call.  The problem this is called multiple times in waterfall as the 
                    # analysis takes place.  Should be called once
                  }
   
   #---------- connect to BondData to run multiple cusips
   
   Cusip <- function(cusip = "character") 
   {
     Cusip.Conn <- gzfile(description = paste("~/BondLab/BondData/",
                                              cusip, ".rds", sep = ""), open = "rb")
     
     Cusip <- readRDS(Cusip.Conn)
     return(Cusip)
   }
   
  