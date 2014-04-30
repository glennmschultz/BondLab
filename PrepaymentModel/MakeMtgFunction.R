#Create the mortgage function class

MortgageRate <- new("MortgageRate",
                 yr30 = function(two = numeric(), ten = numeric()) {
                   2.25 + (.06 * two) + (.75 * ten)
                 },
                 
                 yr15 = function(two = numeric(), ten = numeric()){
                   1.75 + (.06 * two) + (.75 * ten)
                 }
                   )
