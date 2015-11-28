  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # in addition to standard fixed income analysis bond lab provides 
  # for the specific analysis of structured products residential mortgage backed securities, 
  # asset backed securities, and commerical mortgage backed securities
  # License GPL3 + File License
  # Copyright (C) 2014  Glenn M Schultz, CFA

  setClass("Scenario",
         representation(
           Name = "character",
           Type = "character",
           Horizon = "character",
           ShiftType = "character",
           Shiftbps = "numeric",
           Formula = "function"
         ))
  
  setMethod("initialize",
            signature("Scenario"),
            function(.Object,
                     Name = "character",
                     Type = "character",
                     Horizon = "character",
                     ShiftType = "character",
                     Shiftbps = "character",
                     Formula = "function")
              {
              .Object@Name = Name
              .Object@Type = Type
              .Object@Horizon = Horizon
              .Object@ShiftType = ShiftType
              .Object@Shiftbps = Shiftbps
              .Object@Formula = Formula
              
              return(.Object)
            })
  
  RateScenario <- function(
          Name = "character",
          Type = "character",
          Horizon = "character",
          ShiftType = "character",
          Shiftbps = "character",
          Formula = "function")
    {
            new("Scenario",
            Name = "Steepen50",
            Type = "Aggressive",
            Horizon = "Immediate",
            ShiftType = "Twist",
            Shiftbps = c(rep(-25,7), 0, rep(25,3)),
            Formula = function(rates.data, Shiftbps){
            as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
            })}
  
  MakeRateScenario <- function(
    Name = "character",
    Type = "character",
    Horizon = "character",
    ShiftType = "character",
    Shiftbps = "character"){
    
  }