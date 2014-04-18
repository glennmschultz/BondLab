DA100 <- new("Scenario",
             Name = "Down100",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 100,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) - Shiftbps/100)
             })


UA100 <- new("Scenario",
             Name = "UP100",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 100,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
             })


UA150 <- new("Scenario",
             Name = "UP150",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 150,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
             })

saveRDS(UA100, "UA100.rds")

saveRDS(UA150, "UA150.rds")

saveRDS(DA100, "DA100.rds")