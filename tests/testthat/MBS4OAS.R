set.seed(300)
MBS4.OAS <- PassThroughOAS(bond.id = "bondlabMBS4", 
                           trade.date = "01-10-2013", 
                           settlement.date = "01-13-2013", 
                           original.bal = 100000,
                           price = 105.75, 
                           short.rate = 0.0016, 
                           sigma = 0.015, 
                           paths = 200, 
                           PrepaymentAssumption = "MODEL")
equals(object = MBS4.OAS@OAS,
        expected = .00518)