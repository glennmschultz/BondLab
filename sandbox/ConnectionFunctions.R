  
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

  #'@title query sql lite MBS cusip database
  #'@description a function to query the sql lite data base of mortgage
  #'cusips.  The function illustrates how one would connect to a cusip database
  #'and construct the mortgage pass through object for analysis
  #'@param cusip the cusip identifier of the pass through must be in quotes
  #'@importFrom RSQLite SQLite
  #'@importFrom RSQLite dbConnect
  #'@importFrom RSQLite dbSendQuery
  #'@importFrom RSQLite dbBind
  #'@importFrom RSQLite dbFetch
  #'@export MBS
  MBS <- function(cusip){
    MBSData <- dbConnect(SQLite(), dbname= paste0(system.file(package = "BondLab"), "/BondData/MBSData"))
    data <- dbSendQuery(MBSData,
                        'Select *
                        FROM MBS
                        WHERE "cusip" = :x')
    dbBind(data, params = list(x = cusip))
    data <- dbFetch(data)
    MBS <- MBSDetails(
      Cusip = data$Cusip,
      ID = data$PoolId,
      BondType = data$BondType,
      Issuer = data$Issuer,
      Underwriter = data$Issuer,
      Sector = data$Issuer,
      Coupon = data$Coupon,
      IssueDate = data$IssueDate,
      DatedDate = data$DatedDate,
      Maturity = data$MaturityDate,
      LastPmtDate = data$LastPmtDate,
      NextPmtDate = data$NextPmtDate,
      Term = data$Term,
      WALA = data$WALA,
      WAM = data$WAM,
      PaymentDelay = data$PaymentDelay,
      Moody = data$Moody,
      SP = data$SP,
      BondLab = data$BondLab,
      Frequency = data$Frequency,
      BondBasis = as.character(data$BondBasis),
      GWac = data$Gwac,
      OrigLoanBal = data$OrigLoanBal,
      OrigLTV = data$OrigLTV,
      AmortizationType = data$AmortizationType,
      AmortizationTerm = data$AmortizationTerm,
      Index = data$Index,
      Margin = data$Margin,
      FirstPmtDate = data$FirstPmtDate,
      FinalPmtDate = data$FinalPmtDate,
      Servicing = data$Servicing,
      PMI = data$PMI,
      GFeePremium = data$GFeePremium,
      InitialInterest = as.logical(data$InitialInterest),
      InterestOnlyPeriod = data$InterestOnlyPeriod,
      FirstPrinPaymentDate = data$FirstPrinPmtDate,
      BalloonPmt = as.logical(data$BalloonPmt),
      BalloonDate = data$BalloonDate,
      MBSFactor =  data$Factor,
      OriginalBal = data$OriginalBal,
      CurrentBal = data$CurrBalance,
      Model = data$Model,
      Burnout = data$Burnout,
      SATO = data$SATO)
    return(MBS)}
  
  #'@title query sql lite UST Bond cusip database
  #'@description a function to query the sql lite data base of mortgage
  #'cusips.  The function illustrates how one would connect to a cusip database
  #'and construct the mortgage pass through object for analysis
  #'@param cusip the cusip identifier of the pass through must be in quotes
  #'@importFrom RSQLite SQLite
  #'@importFrom RSQLite dbConnect
  #'@importFrom RSQLite dbSendQuery
  #'@importFrom RSQLite dbBind
  #'@importFrom RSQLite dbFetch
  #'@export Bond
  Bond <- function(cusip){
    #create a connection to the MBS database
    BondData <- dbConnect(SQLite(), dbname= paste0(system.file(package = "BondLab"), "/BondData/BondData"))
    data <- dbSendQuery(BondData,
                        'Select *
                        FROM USTBonds
                        WHERE "cusip" = :x')
    dbBind(data, params = list(x = cusip))
    data <- dbFetch(data)
    Bond <- BondDetails(
      Cusip = data$Cusip,
      ID = data$ID,
      BondType = data$BondType,
      Sector = data$Sector,
      Issuer = data$Issuer,
      Underwriter = data$Underwriter,
      OfferAmount = data$OfferAmount,
      Coupon = data$Coupon,
      IssueDate = data$IssueDate,
      DatedDate = data$DatedDate,
      Maturity = data$MaturityDate,
      LastPmtDate = data$LastPmtDate,
      NextPmtDate = data$NextPmtDate,
      Moody = data$Moody,
      SP = data$SP,
      BondLab = data$BondLab,
      Frequency = data$Frequency,
      BondBasis = as.character(data$BondBasis),
      Callable = as.logical(data$Callable),
      Putable = as.logical(data$Putable),
      SinkingFund = as.logical(data$SinkingFund)
    )
    return(Bond)
    
  }


  #' A connection function to BondData folder saved MBS cusip Detail
  #' 
  #' Opens a connection to BondData folder to save MBS cusip detail
  #' @param filename A character string the file name to save the 
  #' default is temp
  #' @export
  SaveMBS <- function(filename = "character"){
    connMBSDetails <- gzfile(
      description = paste(system.file(package = "BondLab"),
                          "/BondData/", filename@ID, ".rds", sep = ""))
      saveRDS(filename, connMBSDetails)
      close(connMBSDetails)}

  
  #' A connection function to BondData folder saved MBS cusip Detail
  #' 
  #' Opens a connection to BondData folder to save bond cusip detail
  #' @param filename A character string the file name to save the 
  #' default is temp
  #' @export
  SaveBond <- function(filename = "character"){
    connBondDetails <- gzfile(
      description = paste(system.file(package = "BondLab"),
                          "/BondData/", filename@ID, ".rds", sep = ""))
    saveRDS(filename, connBondDetails)
    close(connBondDetails)}
  

  #' A connection function to the RatesData folder to call swap curve data
  #' 
  #' A read connection function to the rates database folder to call swap 
  #' rate data
  #' @param trade.date A character string the trade date
  #' @export Rates
  Rates <- function(trade.date = "character"){
    Rates.Conn <-gzfile(description = paste(
      system.file(package = "BondLab"),
      "/RatesData/", as.Date(trade.date,"%m-%d-%Y"), ".rds", sep = ""), 
      open = "rb")
      on.exit(close.connection(Rates.Conn))            
      Rates <- readRDS(Rates.Conn)
      return(Rates)
    }


  #' A connection function to the Prepayment model folder to call 
  #' mortgage rate function class
  #' 
  #' A read connection function to the mortgage rate function. No inputs 
  #' are required the function
  #' propogates the foward mortgage rate based on the 2-year and 10-year
  #' forward rate
  #' @examples
  #' \dontrun{
  #' MtgRate()}
  #' @export
  MtgRate <- function(){
    MtgRate.Conn <- gzfile(description = paste(
      system.file(package = "BondLab"),
      "/PrepaymentModel/MortgageRate.rds", sep = ""), open = "rb")
      MtgRate <- readRDS(MtgRate.Conn)
      on.exit(close.connection(MtgRate.Conn))
      return(MtgRate)}
  
  #' A connection function to save the PrepaymentModel model tuning parameters
  #' 
  #' Opens a connection to the PrepaymentModel folder to save the MortgageRate
  #' object
  #' @export 
  SaveMortgageRate <-function(){
    MortgageRateConn <-gzfile(description = paste(
      system.file(package = "BondLab"), "/PrepaymentModel/", 
      "MortgageRate.rds", sep =""))
    on.exit(close.connection(MortgageRateConn))
    saveRDS("MortgageRate.rds", MortgageRateConn)
  }

  #' A connection function to the horzon MBS pass-though object
  #' 
  #' A read connection function to the horizon MBS details.  
  #' No inputs are required
  #' @examples 
  #' \dontrun{
  #' HorizonMBS()}
  #' @export
  HorizonMBS <- function(){HorizonConn <- gzfile(
    paste(system.file(package = "BondLab"),
          "/Temp_BondData/","TempPassThrough.rds", sep = ""))
   HorizonMBS <- readRDS(HorizonConn)
   on.exit(close.connection(HorizonConn))
   return(HorizonMBS)}
  
  #' A connection function to the horizon bond object
  #' 
  #' A read connection to the horizon bond details
  #' No inputs are required
  #' @examples 
  #' \dontrun{
  #' HorizonBond()}
  #' @export HorizonBond
  HorizonBond <- function(){HorizonConn <- gzfile(
    paste(system.file(package = "BondLab"),
          "/Temp_BondData/","TempBond.rds", sep = ""))
  HorizonBond <- readRDS(HorizonConn)
  on.exit(close.connection(HorizonConn))
  return(HorizonBond)}
 
  #' Function to calculate the updated loan to value
  #' 
  #' A read connection function to the updated loan to value function.  
  #' No inputs are required the function propogates the updated loan to value 
  #' based on amortization and updated home price.
   #' @examples 
   #' \dontrun{
   #' ULTV()}
   #' @export
   ULTV <- function(){
     ULTV.Conn <- gzfile(description = paste(system.file(package = "BondLab"), 
                  "/PrepaymentModel/UpdatedLTV.rds", sep= ""), open = "rb")
     ULTV <- readRDS(ULTV.Conn)
     on.exit(close.connection(ULTV.Conn))
     return(ULTV)
   }
   
  #' A connection function to read the PrepaymentModel model tuning parameters
  #' 
  #' A connection function to the mortgage rate function to call the prepayment 
  #' model tuning
  #' @param bond.id A character string the bond id or cusip currently bond.id 
  #' is supported
  #' @export
   ModelTune <- function(bond.id = "character"){
     ModelTune.Conn <- gzfile(
       description = paste(
         system.file(package = "BondLab"), "/PrepaymentModel/", 
         Model(bond.id),".rds", sep =""), open = "rb")
                ModelTune <- readRDS(ModelTune.Conn)
                on.exit(close.connection(ModelTune.Conn))
                return(ModelTune)
                }

  #' A connection function to save the PrepaymentModel model tuning parameters
  #' 
  #' Opens a connection to the PrepaymentModel folder to save the prepayment
  #' model tuning
  #' @param ModelFile A character string the model tuning default value is temp
  #' @param ModelName A character string the model name used to reference the
  #' prepayment model
  #' @export 
  SaveModelTune <-function(ModelFile = "character", ModelName = "character"){
    ModelTuneConn <-gzfile(description = paste(
      system.file(package = "BondLab"), "/PrepaymentModel/", ModelName, 
      ".rds", sep =""))
    on.exit(close.connection(ModelTuneConn))
    saveRDS(ModelFile, ModelTuneConn)
   }

  #' A connection function read the Prepayment Model Folder ModelFunctions 
  #' class Opens a connection to the PrepaymentModel folder to read the 
  #' ModelFunctions class
  #' @export
  ModelFunctions <- function(){
     ModelFunctionConn <- gzfile(
       description = paste(system.file(package = "BondLab"),
                           "/PrepaymentModel/ModelFunctions.rds", sep =""))
     ModelFunctions <- readRDS(ModelFunctionConn)
     on.exit(close.connection(ModelFunctionConn))
     return(ModelFunctions)
   }

  #' A connection function to save the Prepayment Model ModelFunctions class
  #' 
  #' Opens a connection to the PrepaymentModel folder to save the 
  #' ModelFunctions class
  #' @param ModelFile A character string the model functions class default 
  #' value is temp
  #' @export
  SaveModelFunctions <-function(ModelFile = "character"){
    ModelFunctionConn <-gzfile(
      description = paste(system.file(package = "BondLab"),
                          "/PrepaymentModel/ModelFunctions.rds", sep =""))
     on.exit(close.connection(ModelFunctionConn))
     saveRDS(ModelFile, ModelFunctionConn)
   }

  #' A read connection function to the interest rate scenario data folder
  #' 
  #' Opens connection to the Scenarios folder calling user defined 
  #' scenarios by name
  #' @param Scenario a character string the scenario used in the analysis
  #' @export
  ScenarioCall <- function(Scenario = "character"){
    Scenario.Conn <- gzfile(
      description = paste(system.file(package = "BondLab"), "/Scenario/",
                          as.character(Scenario), ".rds", sep =""), open = "rb")
     scenario <- readRDS(Scenario.Conn)
     on.exit(close.connection(Scenario.Conn))
     return(scenario)}

  #' A save connection function to the interest rate scenario data folder
  #' 
  #' @param Scenario A character string the name of the Scenario
  #' @param ScenarioFile the scenario file to save
  #' @export
  SaveScenario <- function(Scenario = "character", ScenarioFile = "character"){
   connScenario <- gzfile(description = paste(
     system.file(package = "BondLab"),"/Scenario/",Scenario,".rds", sep = ""))
   on.exit(close.connection(connScenario))
   saveRDS(ScenarioFile, connScenario)} 

  #' A read connection function to the REMICData folder
  #' 
  #' Opens a connection to the REMICData folder calling a REMIC Deal by name
  #' this function supports REMIC tranche analysis by providing the REMIC
  #' deal information as the umbrella structure over the tranche
  #' @param remic.deal A character string the REMIC deal name
  #' @export
  REMICDeal <- function(remic.deal = "character") {
    REMIC.Conn <- gzfile(description = paste(
      system.file(package = "BondLab"),"/REMICData/", remic.deal, ".rds", 
      sep = ""), open = "rb")
      REMICDeal <- readRDS(REMIC.Conn)
      on.exit(close.connection(REMIC.Conn))
      return(REMICDeal)
    }

  #' A save connection to the REMICData folder
  #' 
  #' Opens a connection to REMICData folder saving a REMIC Deal by name
  #' this connection is used by the function REMIC structure
  #' @param DealName a character string the Deal Name
  #' @param file a character string the REMIC file name given be constructor
  #' @export
  SaveREMIC <- function(DealName = "character", file = "character"){
    connREMIC <- gzfile(
      description = paste(system.file(package = "BondLab"),"/REMICData/",
                          DealName, ".rds", sep = ""))
      on.exit(close.connection(connREMIC))
      saveRDS(file, connREMIC)
      }

  #' A connection function to the REMICWaterFall source script
  #' 
  #' Opens a connection to the WaterFall folder calling the REMIC waterfall
  #' script by deal name.  This function supports REMIC tranche analysis
  #' @param  deal.name A character string the REMIC deal name
  #' @export  
    REMICWaterFall <- function(deal.name = "character"){
    paste(system.file(package = "BondLab"), "/Waterfall/", deal.name, sep = "")}
   
  #' A connection function to the REMICSchedules Data calls REMIC structuring
  #' element schedules.  This is the PAC/TAC schedule used by the waterfall
  #' Opens connection to the Schedules folder by REMIC deal name
  #' this function supports REMIC tranche analysis by provding the REMIC
  #' schedules deal information
  #' @param REMIC.Tranche the REMIC tranche MBS.id or cusip.
  #' Currently MBS.id is supported
  #' @export
  REMICSchedules <- function(REMIC.Tranche = "character"){
    Sched.Conn <- gzfile(
      paste(system.file(package = "BondLab"),"/Schedules/", 
            as.character(REMIC.Tranche@DealName), "_Group_", 
            as.numeric(REMIC.Tranche@Group), "_Sch", ".rds", sep = ""), 
      open = "rb")
      REMICSchedules <- readRDS(Sched.Conn)
      on.exit(close.connection(Sched.Conn))
      return(REMICSchedules) 
      }

  #' A connection function to the schedules folder
  #' 
  #' Opens a connection to the Schedules folder to save PAC or TAC schedules
  #' @param DealName A character string the deal name
  #' @param ScheduleFile A character string the name of the 
  #' schedule file to save
  #' @export
    SaveSchedules <- function(
      DealName = "character", 
      ScheduleFile = "character"){
      connSched <- gzfile(
        description = paste(
          system.file(package = "BondLab"), "/Schedules/", 
          DealName,"_","Group","_",ScheduleFile@Group,"_", "Sch", ".rds", 
          sep = ""))
      on.exit(close.connection(connSched))
      saveRDS(ScheduleFile, connSched)
    }

  #' A connection function to save the RAID information
  #' 
  #' Opens a connection to the RAID folder and saves RAID information
  #' REMIC at Issuance Disclouse data
  #' @param RAIDFile a character string the REMIC At Issuance Disclosure
  #' @export
  SaveRAID <- function(RAIDFile = "character"){  
    connRAID <- gzfile(
      description = paste(
        system.file(package = "BondLab"), "/RAID/", 
        RAIDFile@DealName,".rds", sep = ""))
    on.exit(close.connection(connRAID))
    saveRDS(RAIDFile, connRAID)
    }

  #' A connection function to read the RAID information
  #' 
  #' Opens a connection to the RAID folder and saves RAID information
  #' REMIC at Issuance Disclouse data
  #' @param RAIDFile a character string the REMIC At Issuance Disclosure
  #' @export
  ReadRAID <- function(RAIDFile = "character"){  
    connRAID <- gzfile(
      description = paste(
        system.file(package = "BondLab"), "/RAID/",RAIDFile,".rds", sep = ""))
    RAID <- readRDS(connRAID)
    on.exit(close.connection(connRAID))
    return(RAID)
    }

  #' A connection function to save Tranche Details information
  #' 
  #' Opens a connection to the Tranches folder and save Tranche Data
  #' REMIC Tranche information
  #' @param DealName a character string the Deal Name
  #' @param TrancheNumber a character string the Tranche number
  #' @param TrancheFile a character string the TrancheDetails 
  #' object (file) to serialize
  #' @export
  SaveTranche <- function(
    DealName = "character", 
    TrancheNumber = "character", 
    TrancheFile = "character"){
    connTranche <- gzfile(
      description = paste(
        system.file(package = "BondLab"), "/Tranches/",DealName,"_",
        "Tranche","_",TrancheNumber,".rds", sep = ""))
    on.exit(close.connection(connTranche))
    saveRDS(TrancheFile, connTranche)
  }
  
  #' A connection function used to assemble the deal tranches as a list
  #' 
  #' Opens a helper connection to the Tranches folder for the REMIC constructor
  #' used by the REMIC constructor function to aggregate Tranche data
  #' @param DealName A character string the Deal Name
  #' @param TrancheNumber A character string the tranche number
  #' @export
  SaveTranches <- function(
    DealName = "character", 
    TrancheNumber = "character"){
    connTranches <- gzfile(
      description = paste(
        system.file(package = "BondLab"),"/Tranches/",
        DealName,"_","Tranche", "_",TrancheNumber,".rds", sep = "")) 
    Tranches <- readRDS(connTranches)
    on.exit(close.connection(connTranches))
    return(Tranches)}

  #' A connection functon to the Groups folder
  #' 
  #' Opens a connection to the Groups folder to save collateral group
  #' @param FileName A character string the FileName default to temp
  #' @param DealName A character string the DealName
  #' @param Group A numeric value the Group number
  #' Note: Group is a counter for this function connection used in the 
  #' CollateralGroup function.  
  #' @export
  SaveCollGroup <- function(
    FileName = "character", 
    DealName = "character", 
    Group = numeric()){
    connGroup <- gzfile(
      description = paste(
        system.file(package = "BondLab"),"/Groups/",
        DealName,"_","Group","_",FileName@Group,".rds", sep = ""))
    on.exit(close.connection(connGroup))
    saveRDS(FileName, connGroup)
  }

  #' A connection function to the groups folder
  #' 
  #' The connection is a read helper to the aggregator CollateralGroup 
  #' @param DealName A character string the DealName
  #' @param Group A numeric value the collateral group number.  
  #' Note: Group is a counter for this function connection used in 
  #' the CollateralGroup function
  #' @export
  REMICGroupConn <- function(
    DealName = "character", 
    Group = numeric()){
    REMICGrpConn <- gzfile(
      description = paste(system.file(package = "BondLab"),"/Groups/",
                          DealName,"_","Group","_",Group,".rds", sep = ""))
  }

  #' A save connection to the RDME folder 
  #' 
  #' The function opens a connection to the RDME file to save
  #' REMIC Month End Disclosure data
  #' @param FileName A character string the FileName default to temp
  #' @param DealName a character string the DealName
  #' @param TrancheNumber a numeric value the tranche number
  #' @export
  SaveRDME <- function(
    FileName = "character", 
    DealName = "character", 
    TrancheNumber = numeric()){
    connRDME <- gzfile(
      description = paste(system.file(package = "BondLab"),
                          "/RDME/",DealName,"_","Tranche","_",TrancheNumber,"_"
                          ,"Factor",".rds", sep = ""))
  saveRDS(FileName, connRDME)
  close(connRDME)
  }

  #' A read connection to RDME folder
  #' 
  #' The function opens a connection for the aggreation of factor data
  #' which is used by the REMIC constructor
  #' @param DealName a character string the Deal Name
  #' @param TrancheNumber a numeric value the tranche number
  #' @export 
  RDMEFactor <- function(
    DealName = "character", 
    TrancheNumber = numeric()){
    connRDME <- gzfile(
      description = paste(
        system.file(package = "BondLab"), "/RDME/", DealName,"_","Tranche","_",
        TrancheNumber,"_","Factor",".rds", sep = "")) 
  ReadRDME <- readRDS(connRDME)
  on.exit(close.connection(connRDME))
  return(ReadRDME)
  }
