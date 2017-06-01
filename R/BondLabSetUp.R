  
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


  #' Setup function for the R package BondLab to create the needed objects
  #' 
  #' @export
  BondLabSetUp <- function(){
    
    do.call(source, 
            list(file = paste(system.file(package = "BondLab"), 
                              "/Scenario/Scenario_SpotCurve", sep = ""),
                 local = TRUE))
    do.call(source,
            list(file = paste(system.file(package = "BondLab"), 
                              "/Scenario/Scenario_YieldCurve", sep = ""),
                 local = TRUE))
    #do.call(source,
    #        list(file = paste(system.file(package = "BondLab"), 
    #                          "/BondData/BondData", sep = ""),
    #             local = TRUE))
  }