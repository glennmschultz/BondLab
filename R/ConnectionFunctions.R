  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Glenn M Schultz, CFA
  # Fair use of the Bond Lab trademark is limited to promotion of the use of Bond Lab software or 
  # the book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

  #--------------------------------------------------------------------------------
    #' A connection function to the RatesData folder to call swap curve data
    #' 
    #' A read connection function to the rates data base folder to call swap rate data
    #' @param trade.date A character string the trade date
    #' @export Rates
    Rates <- function(trade.date = "character"){
      
      Rates.Conn <-gzfile(description = paste(system.file(package = "BondLab"),
                  "/RatesData/", as.Date(trade.date, "%m-%d-%Y"), ".rds", sep = ""), open = "rb")
                  
                  Rates <- readRDS(Rates.Conn)
                  return(Rates)
                  }
    setGeneric("Rates", function(trade.date = "character")
      {standardGeneric("Rates")})
   
   #---------------------------------------------------------------------------------
    #' A connection function to the Prepayment model folder to call mortgage rate function class
    #' 
    #' A read connection function to the mortgage rate function. No inputs are required the function
    #' propogates the foward mortgage rate based on the 2-year and 10-year forward rate
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
   
   #----------------------------------------------------------------------------------
   #' Function to calculate the updated loan to value
   #' 
   #' A read connection function to the updated loan to value function.  No inputs are
   #' required the function propogates the updated loan to value based on amortization
   #' and updated home price.
   #' @examples 
   #' \dontrun{
   #' ULTV()}
   #' @export
   ULTV <- function(){
    
     ULTV.Conn <- gzfile(description = paste(system.file(package = "BondLab"), 
                  "/PrepaymentModel/UpdatedLTV.rds", sep= ""), open = "rb")
     
     ULTV <- readRDS(ULTV.Conn)
                  }
   
  #-----------------------------------------------------------------------------------
  #' A connection function to call the PrepaymentModel model tuning parameters
  #' 
  #' A connection function to the mortgage rate function to call prepayment model tuning
  #' @param bond.id A character string the bond id or cusip currently bond.id is supported
  #' @export
   ModelTune <- function(bond.id = "character")
                 {
                 ModelTune.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                                  "/PrepaymentModel/", bond.id@Model,".rds", sep =""), open = "rb")
                 ModelTune <- readRDS(ModelTune.Conn)    
                 }

  #------------------------------------------------------------------------------------
  #' A connection function to the BondData to calling MBS cusips
  #' 
  #' Opens a connection to the BondData folder to call MBS cusip data 
  #' @param MBS.id A character string the MBS.id or cusip number current MBS.id is supported
  #' @export
  MBS <- function(MBS.id = "character"){
                  MBS.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                              "/BondData/", MBS.id, ".rds", sep = ""), open = "rb")          
                  MBS <- readRDS(MBS.Conn)
                  return(MBS)
                  }
    
    #----------------------------------------------------------------------------------
    #' A connection function to the BondData calling bond cusips
    #' 
    #' Opens a connection to BondData folder to call a standard bond
    #' @param Bond.id A character string the bond's cusip number or id
    #' @export
    Bond <- function(Bond.id = "character")
    {
      Bond.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                  "/BondData/", Bond.id, ".rds", sep = ""), open = "rb")
      Bond <- readRDS(Bond.Conn)
      return(Bond)
    }  
     
  #-------------------------------------------------------------------------------------
  #' A connection function to the REMICData calls REMICs by deal
  #' 
  #' Opens a connection to the REMICData folder calling a REMIC Deal by name
  #' this function supports REMIC tranche analysis by providing the REMIC
  #' deal information as the umbrella structure over the tranche
  #' @param remic.deal A character string the REMIC deal name
  #' @export
    REMICDeal <- function(remic.deal = "character") {
                  
      REMIC.Conn <- gzfile(description = paste(system.file(package = "BondLab"),
                    "/REMICData/", remic.deal, ".rds", sep = ""), open = "rb")
      REMICDeal <- readRDS(REMIC.Conn)
      return(REMICDeal)
                  }
   
  #-------------------------------------------------------------------------------------
  #' A connection function to the REMICWaterFall source script
  #' 
  #' Opens a connection to the WaterFall folder calling the REMIC waterfall
  #' script by deal name.  This function supports REMIC tranche analysis
  #' @param  deal.name A character string the REMIC deal name
  #' @export  
   REMICWaterFall <- function(deal.name = "character"){
    paste(system.file(package = "BondLab"), "/Waterfall/", deal.name, sep = "")}
   
  #------------------------------------------------------------------------------------- 
  #' A connection function to the REMICSchedules Data calls REMIC structuring element
  #' schedules.  This is the PAC/TAC schedule used by the waterfall
  #' 
  #' Opens connection to the Schedules folder by REMIC deal name
  #' this function supports REMIC tranche analysis by provding the REMIC
  #' schedules deal information
  #' @param REMIC.Tranche the REMIC tranche MBS.id or cusip.  Currently MBS.id is supported
  #' @export
    REMICSchedules <- function(REMIC.Tranche = "character"){
      
      Sched.Conn <- gzfile(paste(system.file(package = "BondLab"),
                    "/Schedules/", as.character(REMIC.Tranche@DealName), 
                    "_Group_", as.character(REMIC.Tranche@Group), "_Sch", ".rds"), open = "rb")
      REMICSchedules <- readRDS(Sched.Conn)
      return(REMICSchedules) 
      }
   
  #---------------------------------------------------------------------------------------
  #' A connection function to the interest rate scenario data folder
  #' 
  #' Opens connection to the Scenarios folder calling user defined scenarios by name
  #' @param Scenario a character string the scenario used in the analysis
  #' @export
  ScenarioCall <- function(Scenario = "character"){
    Scenario.Conn <- gzfile(description = paste(system.file(package = "BondLab"), "/Scenario/", 
                      as.character(Scenario), ".rds", sep =""), open = "rb")
    scenario <- readRDS(Scenario.Conn)
    return(scenario)}
  
  #----------------------------------------------------------------------------------------
  #' A connection function to save the RAID information
  #' 
  #' Opens a connection to the RAID folder and saves RAID information
  #' REMIC at Issuance Disclouse data
  #' @param RAIDFile a character string the REMIC At Issuance Disclosure
  #' @export
  SaveRAID <- function(RAIDFile = "character"){  
    connRAID <- gzfile(description = paste(system.file(package = "BondLab"), "/RAID/",RAIDFile@DealName,".rds", sep = ""))
    saveRDS(RAIDFile, connRAID)
    close(connRAID)}
  
  #----------------------------------------------------------------------------------------
  #' A connection function to the Tranche Details information
  #' 
  #' Opens a connection to the Tranches folder and save Tranche Data
  #' REMIC Tranche information
  #' @param Tranche a character string the Tranche
  #' @export
  SaveTranche <- function(Tranche = character()){
    connTranche <- gzfile(description = paste(system.file(package = "BondLab"),
                                  "~/BondLab/Tranches/",DealName,"_","Tranche","_",temp@TrancheNumber,".rds", sep = ""))
    saveRDS(temp, connTranche)
    close(connTranche)
  }
  