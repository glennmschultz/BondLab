
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.

  
  #'@title Split Bond to Floater Inverse Floater
  #'@description Structuring function to split a bond into a floater and 
  #'inverse floating rate bond.  The optional arguments will determine the principal
  #'balance of the floating rate bond.  Select either inverse.multiplier or inverse.percentage
  #'@param floater.margin the floater rate bond's margin to index
  #'@param bond.coupon the coupon of the parent bond
  #'@param bond.balance the principal balance of the parent bond
  #'@param index the floating rate index to which the floating bond is struck
  #'@param ..., optional arguments follow select either inverse.muliplier or inverse.percentage
  #'@param inverse.multiplier the inverse bond's multipler to the floating index
  #'@param inverse.percentage the percentage balance of the inverse bond relative to the 
  #'parent bond
  #'@family Structuring
  #'@export SplitFloaterInverse
  SplitFloaterInverse <- function(floater.margin, 
                                 bond.coupon, 
                                 bond.balance, 
                                  index,
                                  ...,
                                  inverse.multiplier = NULL,
                                  inverse.percentage = NULL){
  
    if(is.null(inverse.multiplier) == FALSE & is.null(inverse.percentage) == FALSE){
     stop('you cannot specify both inverse multiplier and floater percentage')
   }
  
   if(is.null(inverse.percentage) == FALSE) {inverse.multiplier = 
      (bond.balance/(bond.balance * inverse.percentage)) - 1
   } else {inverse.multiplier = inverse.multiplier}
  
  
    floater.cap = bond.coupon + (bond.coupon/inverse.multiplier)
    if(floater.cap <= max(floater.margin, bond.coupon)) stop('cap is too low')
    floater.balance = (inverse.multiplier * bond.balance)/(1 + inverse.multiplier)
    inverse.balance = bond.balance -floater.balance
    inverse.cap = (
      (bond.coupon * bond.balance) - (floater.margin * floater.balance)
    )/ inverse.balance
  
    structure = list(
      floater.balance = floater.balance,
      floater.cap = floater.cap,
      floater.floor = floater.margin,
      floater.index = index,
      floater.formula = eval(parse(text = paste0('function(',index,'){min(',round(floater.cap,2),
                                                 ',max((',index,'+',floater.margin,'),',
                                                floater.margin,'))}'))),
      inverse.balance = inverse.balance,
      inverse.cap = inverse.cap,
      inverse.multipler = inverse.multiplier,
      inverse.floor = 0,
      inverse.floater.formula = eval(parse(text = paste0('(function(',index,')', '{min(',round(inverse.cap,2),
                                                        ',max((',-inverse.multiplier,'*',index,')+',
                                                        round(inverse.cap,2),',0))})')))
    )
    return(structure)
}