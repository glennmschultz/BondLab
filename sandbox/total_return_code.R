
  horizon.months <- as.character(seq(as.Date(Curve[1,1]), by = 'months', length.out = 12), format ='%Y-01-%m')
  coupon.months <- as.character(as.Date(PmtDate(cashflow)), format = "%Y-01-%m")
  prin.months <- as.character(as.Date(PmtDate(cashflow)), format = "%Y-01-%m")
  
  CashFlowArray <- array(data = 0, dim = c(4,13), dimnames = NULL)
  
  colindex = NULL
    for(col in seq_along(coupon.months)){
    loc <- which(as.Date(coupon.months[col]) == as.Date(horizon.months))
    if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
      } else {next()} 
    }

  for(pmtrow in seq_along(colindex)){
    CashFlowArray[pmtrow,colindex[pmtrow]] <- CouponPmt(cashflow)[pmtrow]}
  
  #-----------------------------------------------------------------------------
  #principal payment in the cash flow arrary this will work with non callable
  #non sinking fund bonds.  The bond cash flow engine should be upgraded to
  #principal paid
  
  colindex = NULL
  for(col in seq_along(prin.months)){
    loc <- which(as.Date(prin.months[col]) == as.Date(horizon.months))
    if(length(loc) != 0){colindex <- append(colindex, loc, after = length(colindex))
    } else {next()} 
  }

  for(col in seq_along(colindex)){
    CashFlowArray[3,colindex[col]] <- OfferAmount(bond) -PrincipalOutstanding(cashflow)[3]}
  
  #----------------------------------------------------------------------------
  #allocate reinvestment to cashflows
  reinvest = 0.0025
  for(rr in 1:nrow(CashFlowArray-1)){
    for(month in 1:12){
      if(month == 1){CashFlowArray[rr,month] = CashFlowArray[rr,month]
      } else {CashFlowArray[rr,month] = CashFlowArray[rr,month] + CashFlowArray[rr, month-1] *(1 + reinvest)}
      print(CashFlowArray[rr,month])
    }
  }
  
  
  CashFlowArray
  nrow(CashFlowArray)
  sum(CashFlowArray[2,])