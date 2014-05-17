DA100 <- new("Scenario",
             Name = "Down100",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 100,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) - Shiftbps/100)
             })

DA50  <- new("Scenario",
             Name = "Down50",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 50,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) - Shiftbps/100)
             })

DA25  <- new("Scenario",
             Name = "Down50",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 25,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) - Shiftbps/100)
             })

NC  <- new("Scenario",
             Name = "NoChg",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 0,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) - Shiftbps/100)
             })

UA25 <- new("Scenario",
            Name = "UP50",
            Type = "Aggressive",
            Horizon = "Immediate",
            ShiftType = "Parallel",
            Shiftbps = 25,
            Formula = function(rates.data, Shiftbps){
              as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
            })
              
UA50 <- new("Scenario",
             Name = "UP50",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 50,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
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

UA200 <- new("Scenario",
             Name = "UP200",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 200,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
             })

UA250 <- new("Scenario",
             Name = "UP250",
             Type = "Aggressive",
             Horizon = "Immediate",
             ShiftType = "Parallel",
             Shiftbps = 250,
             Formula = function(rates.data, Shiftbps){
               as.character(as.numeric(rates.data[1,2:length(rates.data)]) + Shiftbps/100)
             })

saveRDS(DA100, "DA100.rds")
saveRDS(DA50, "DA50.rds")
saveRDS(DA25, "DA25.rds")
saveRDS(NC, "NC.rds")
saveRDS(UA25, "UA25.rds")
saveRDS(UA50, "UA50.rds")
saveRDS(UA100, "UA100.rds")
saveRDS(UA150, "UA150.rds")
saveRDS(UA200, "UA200.rds")
saveRDS(UA250, "UA250.rds")