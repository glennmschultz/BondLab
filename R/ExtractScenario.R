

  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed securities
  # Copyright (C) 2016  Bond Lab Technologies, Inc.



  ExtractScenario <- function(bond.id = character){
  
  ScenarioName = list()
  ScenarioBPS = list()
  ScenarioCPR = list()
  ScenarioYieldToMaturity = list()
  ScenarioSpreadToCurve = list()
  ScenarioWAL = list()
  ScenarioModDuration = list()
  ScenarioHorizonReturn = list()
  
  for(i in 1:as.numeric(length(bond.id@Scenario))){
    ScenarioName = append(ScenarioName,bond.id@Scenario[i][[1]]@Name)
    ScenarioBPS = append(ScenarioBPS, bond.id@Scenario[i][[1]]@Shiftbps)
    ScenarioCPR = append(ScenarioCPR,  SMMVector.To.CPR(bond.id@Scenario[i][[1]]@SMM, length(bond.id@Scenario[i][[1]]@SMM)) * 100)  
    ScenarioYieldToMaturity = append(ScenarioYieldToMaturity, bond.id@Scenario[i][[1]]@YieldToMaturity * 100)
    ScenarioSpreadToCurve = append(ScenarioSpreadToCurve, bond.id@Scenario[i][[1]]@SpreadToInterCurve)
    ScenarioWAL = append(ScenarioWAL, bond.id@Scenario[i][[1]]@WAL)
    ScenarioModDuration = append(ScenarioModDuration, bond.id@Scenario[i][[1]]@ModDuration)
    ScenarioHorizonReturn = append(ScenarioHorizonReturn, bond.id@Scenario[i][[1]]@HorizonReturn)
    
  }
  Result <- cbind(ScenarioName, ScenarioBPS, ScenarioCPR, ScenarioYieldToMaturity, ScenarioSpreadToCurve, ScenarioWAL, ScenarioModDuration, ScenarioHorizonReturn)
  return(Result)
  }