# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + File License
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
# the book "Investing in Mortgage Backed Securities Using Open Source Analytics" 



  #---------- connect to the rates data folder
    #' A connection function to the RatesData folder to call swap curve data
    #' 
    #' A read connection function to the rates data base folder to call swap rate data
    #' @param trade.date A character string the trade date
    #' @export Rates
    Rates <- function(trade.date = "character"){
      
      Rates.Conn <-gzfile(description = paste(system.file(package = "BondLab"),
                  "/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
      
      #Rates.Conn <-gzfile(description = paste("~/BondLab/RatesData/", 
      #            as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
                  
                  Rates <- readRDS(Rates.Conn)
                  return(Rates)
                  }
    setGeneric("Rates", function(trade.date = "character")
      {standardGeneric("Rates")})
   
   #-------- connect to mortgage rate function classes
    #' A connection function to the Prepayment model folder to call mortgage rate function class
    #' 
    #' A read connection function to the mortgage rate function. No inputs are required
    #' @examples
    #' \dontrun{
    #' MtgRate()}
    #' @export
   MtgRate <- function(){
     
     MtgRate.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                      "/PrepaymentModel/MortgageRate.rds", sep = ""), open = "rb")
      #MtgRate.Conn <- gzfile("~/BondLab/PrepaymentModel/MortgageRate.rds", open = "rb")
      MtgRate <- readRDS(MtgRate.Conn)
                   }
   
   #------- connect to updated LTV function class
   # not currently implemented
   ULTV <- function(){
    
     ULTV.Conn <- gzfile(description = paste(system.file(package = "BondLab"), 
                  "/PrepaymentModel/UpdatedLTV.rds", sep= ""), open = "rb")
     
    #ULTV.Conn <- gzfile("~/BondLab/PrepaymentModel/UpdatedLTV.rds", open = "rb")
     ULTV <- readRDS(ULTV.Conn)
                  }
   
  #----------- connect to prepayment model tune data folder
  #' A connection function to the PrepaymentModel folder to call the prepayment model tuning
  #' 
  #' A connection function to the mortgage rate function to call prepayment model tuning
  #' @param bond.id A character string the bond id or cusip
  #' @export
   ModelTune <- function(bond.id = "character")
                 {
                 ModelTune.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                                  "/PrepaymentModel/", bond.id@Model,".rds", sep =""), open = "rb")
                 
                 #ModelTune.Conn <- gzfile(description = paste("~/BondLab/PrepaymentModel/", 
                 #                   bond.id@Model, ".rds", sep =""), open = "rb")        
                 ModelTune <- readRDS(ModelTune.Conn)    
                 }

  #----------- connect to MBS cusip data folder
    #' A connection function to the BondData folder to call the MBS cusip objects
    #' 
    #' Opens a connection to the BondData folder to call MBS cusip data 
    #' @param MBS.id A character string the MBS id or cusip number
    #'@export
    MBS <- function(MBS.id = "character")
                  {
                  MBS.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                              "/BondData/", MBS.id, ".rds", sep = ""), open = "rb")          
      
      
                  #MBS.Conn <- gzfile(description = paste("~/BondLab/BondData/",
                  #                                       MBS.id, ".rds", sep = ""), open = "rb")
                  MBS <- readRDS(MBS.Conn)
                  return(MBS)
                  }
    
    #----------- connect to Bond cusip data folder
    #' A connection function to the BondData folder for standard bond
    #' 
    #' Opens a connection to BondData folder to call a standard bond
    #' @param Bond.id A character string the bond's cusip number or id
    #' @export
    Bond <- function(Bond.id = "character")
    {
      Bond.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                  "/BondData/", Bond.id, ".rds", sep = ""), open = "rb")
      
      #Bond.Conn <- gzfile(description = paste("~/BondLab/BondData/",
      #            Bond.id, ".rds", sep = ""), open = "rb")
      Bond <- readRDS(Bond.Conn)
      return(Bond)
    }  
     
  #----------- connect to REMIC deal file
  #' A connection function to the REMICData folder to call REMICs by deal
  #' 
  #' Opens connection to the REMICData folder calling a REMIC Deal by name
  #' @param remic.deal A character string the REMIC deal name
  #' @export
    REMICDeal <- function(remic.deal = "character") {
                  
      REMIC.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                    "/REMICData/", remic.deal, ".rds", sep = ""), open = "rb")
      
      #REMIC.Conn <- gzfile(description = paste("~/BondLab/REMICData/",
      #remic.deal, ".rds", sep = ""), open = "rb")
                  
      REMICDeal <- readRDS(REMIC.Conn)
      return(REMICDeal)
                  }
   
   #---------- connect to waterfall data
   REMICWaterFall <- function(deal.name = "character"){
    paste(system.file(package = "BondLab"), "/Waterfall/", deal.name, sep = "") 
    #paste("~/BondLab/WaterFall/",deal.name, sep = "")
                  }
   
   #---------- connect to retrieve bond schedule data 
    REMICSchedules <- function(REMIC.Tranche = "character"){
      
      Sched.Conn <- gzfile(paste(system.file(package = "BondLab"),
                    "/Schedules/", as.character(REMIC.Tranche@DealName), 
                    "_Group_", as.character(REMIC.Tranche@Group), "_Sch", ".rds"), open = "rb")
      
        Sched.Conn <- gzfile(paste("~/BondLab/Schedules/",
        as.character(REMIC.Tranche@DealName), "_Group_", as.character(REMIC.Tranche@Group ), "_Sch", ".rds",
        sep = ""), open = "rb")
                    
      REMICSchedules <- readRDS(Sched.Conn)
      #return(REMICSchedules) No need to return as this is not saved via a function wrapper but used
      # in the waterfall call.  I think this is called multiple times should be called once trace this?
      }
   
   #---------- connect to BondData to run multiple cusips
   # BondLab will need a connection string to a cusip file to run multiple cusips from 
   # a collateral group.  Perhaps create a Group_Collateral_File
  