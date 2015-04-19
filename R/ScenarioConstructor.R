# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3 + License File
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

#======================  Set and create class Scenario ==============================
setMethod("initialize",
          signature("Scenario"),
          function(.Object,
                   Name = "character",
                   Type = "character",
                   Horizon = "character",
                   ShiftType = "character",
                   Shiftbps = "numeric",
                   Formula = "function")
          {
          .Object@Name = Name
          .Object@Type = Type
          .Object@Horizon = Horizon
          .Object@ShiftType = ShiftType
          .Object@Shiftbps = Shiftbps
          .Object@Formula = Formula
          
          return(.Object)
          callNextMethod(.Object,...)
          })

Scenario <- function(
  Name = "character",
  Type = "character",
  Horizon = "character",
  ShiftType = "character",
  Shiftbps = "numeric",
  Formula = "function"){
  
  new("Scenario",
    Name = Name,
    Type = Type,
    Horizon = Horizon,
    ShiftType = ShiftType,
    Shiftbps = Shiftbps,
    Formula = Formula)
}

# ========================== This function constructs and saves Scenario Object ================
#' A constructor funtion of the class MakeScenario
#' 
#' This is a standard generic function used to construct the class Scenario.
#' The Scenario class is applied to the yield curve and is passed to the scenario.set variable
#' 
#' @param Name A character string indicating the unique name which identifies the scenario
#' @param Type A character string indicating the type of scenario "aggressive", "moderate"
#' @param Horizon A character string indicating the time horizon over wich the scenario evolves
#' @param ShiftType A character sting indicating the type interest rate shift "Parallel", "Twist", "etc"
#' @param Shiftbps A numeric vector indicating the shift applied to each point on the curve
#' @param Formula A function to apply the shift to each pont on the curve
#' @examples
#' \dontrun{
#' MakeScenario(Name = "Flat50",
#' Type = "Agrressive",
#' Horizon = "Immediate",
#' ShiftType = "Twist",
#' Shiftbps = c(rep(25,7),0, rep(-25,3)),
#' Formula = function(rates.data, Shiftbps){
#' as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)})}  
#' @export
  MakeScenario <- function(
    Name = "character",
    Type = "character",
    Horizon = "character",
    ShiftType = "character",
    Shiftbps = "numeric",
    Formula = "function")
  {
    temp <- Scenario(
      Name = Name,
      Type = Type,
      Horizon = Horizon,
      ShiftType = ShiftType,
      Shiftbps = Shiftbps,
      Formula = Formula)
    
    connScenario <- gzfile(description = paste("~/BondLab/Scenario/",temp@Name,".rds", sep = ""))
    saveRDS(temp, connScenario)
    close(connScenario)
  }

  setGeneric("MakeScenario", function(
                            Name = "character",
                            Type = "character",
                            Horizon = "character",
                            ShiftType = "character",
                            Shiftbps = "numeric",
                            Formula = "function")
    {standardGeneric("MakeScenario")})