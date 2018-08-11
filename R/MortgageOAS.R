
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

  # The following script is used to calculate Term Structure
  # metrics for mortgage backed securities.  To create the script
  # the standard procedure is followed set class, set generics,
  # set methods, functions.  This class is a sub class (document superclass)

  #'@include CashFlowEngine.R PrepaymentModel.R MBSDetails.R CurveSpreads.R
  NULL
  
  #'@title mortgage oas class
  #'@family mortgage pass through option adjusted spread
  #'@description mortgage pass through oas class
  #'@slot Cusip the pass through cusip
  #'@slot Issuer the pass through issuer
  #'@slot Coupon the pass through investor coupon
  #'@slot Term the pass though amortization term 
  #'@slot OAS the mortgage option adjusted spread
  #'@slot ZeroVolSpread the zero volatility spread
  #'@slot Spreads the path spread along each simulation
  #'@slot OAD option adjusted duration
  #'@slot OAC option adjusted convexity
  #'@exportClass MortgageOAS
  setClass('MortgageOAS',
           representation(
             Cusip = 'character',
             Issuer = 'character',
             Coupon = 'numeric',
             Term = 'numeric',
             OAS = 'numeric',
             ZeroVolSpread = 'numeric',
             Spreads = 'numeric',
             OAD = 'numeric',
             OAC = 'numeric'
           ))
  
  #'@title generic function to plot OAS
  #'@description standard generic to polt MortgageOAS
  #'@param object MortgageOAS object
  #'@export PlotOAS
  setGeneric('PlotOAS', function(object)
  {standardGeneric('PlotOAS')})
  
  # Note standard generic Cusip is defined in MBSDetails.R
  # Note standard generic Issuer is defined in MBSDetails.R
  # Note standard generic Coupon is defined in MBSDetails.R
  # Note standard generic Term is defined in MBSDetails.R
  # Note standard generic ZeroVolSpread is defined in CurveSpreads.R

  #'@title A standard generic to access the slot OAS
  #'@description Access the slot OAS from object MortgageOAS
  #'@param object MortgageOAS
  #'@export OAS
  setGeneric('OAS', function(object)
    {standardGeneric('OAS')})
  
  #'@title A standard generic to access the slot Spreads
  #'@description Access the slot Spreads from object MortgageOAS
  #'@param object MortgageOAS
  #'@export Spreads
  setGeneric('Spreads', function(object)
    {standardGeneric('Spreads')})
  
  #'@title A standard generic to access the slot OAD
  #'@description Access the slot OAD from object MortgageOAS
  #'@param object MortgageOAS
  #'@export OAD
  setGeneric('OAD', function(object)
    {standardGeneric('OAD')})
  
  #'@title A standard generic to access the slot OAC
  #'@description Access the slot OAC from object MortgageOAS
  #'@param object MortgageOAS
  #'@export OAC
  setGeneric('OAC', function(object)
    {standardGeneric('OAC')})
  
  setMethod('initialize',
            signature('MortgageOAS'),
            function(.Object,
                     Cusip = 'character',
                     Issuer = 'character',
                     Coupon = numeric(),
                     Term = numeric(),
                     OAS = numeric(),
                     ZeroVolSpread = numeric(),
                     Spreads = numeric(),
                     OAD = numeric(),
                     OAC = numeric(),
                     ...){
              callNextMethod(.Object,
                             Cusip = Cusip,
                             Issuer = Issuer,
                             Coupon = Coupon,
                             Term = Term,
                             OAS = OAS,
                             ZeroVolSpread = ZeroVolSpread,
                             Spreads = Spreads,
                             OAD = OAD,
                             OAC = OAC,
                             ...)
            })
  
  #'@title A method to access the slot cusip
  #'@description Access the slot Cusip from object MortgageOAS
  #'@param object MortgageOAS object
  #'@exportMethod Cusip
  setMethod('Cusip', signature('MortgageOAS'),
            function(object){object@Cusip})
  
  #'@title A method to access the slot Issuer
  #'@description Access the slot Issuer from object MortgageOAS
  #'@param object MortgageOAS object
  #'@exportMethod Issuer
  setMethod('Issuer', signature('MortgageOAS'),
            function(object){object@Issuer})
  
  #'@title A Method to access the slot Coupon
  #'@description Access the slot Coupon from object MortgageOAS
  #'@param object MortgageOAS object
  #'@exportMethod Coupon
  setMethod('Coupon', signature('MortgageOAS'),
            function(object){object@Coupon})
  
  #'@title A Method to access the slot Term
  #'@description Access the slot Term from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod Term
  setMethod('Term', signature('MortgageOAS'),
            function(object){object@Term})
  
  #'@title A Method to access the slot OAS
  #'@description Access the slot OAS from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod OAS
  setMethod('OAS', signature('MortgageOAS'),
            function(object){object@OAS})
  
  #'@title A Method to access the slot ZeroVolSpread
  #'@description Access the slot ZeroVolSpread from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod ZeroVolSpread
  setMethod('ZeroVolSpread', signature('MortgageOAS'),
            function(object){object@ZeroVolSpread})
  
  #'@title A Method to access the slot Spreads
  #'@description Access the slot Spreads from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod Spreads
  setMethod('Spreads', signature('MortgageOAS'),
            function(object){object@Spreads})
  
  #'@title A Method to access the slot OAD
  #'@description Access the slot OAD from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod OAD
  setMethod('OAD', signature('MortgageOAS'),
            function(object){object@OAD})
  
  #'@title A Method to access the slot OAC
  #'@description Access the slot OAC from object MortgageOAS
  #'@param object MortgageOAS
  #'@exportMethod OAC
  setMethod('OAC', signature('MortgageOAS'),
            function(object){object@OAC})
  