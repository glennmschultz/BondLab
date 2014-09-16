# Bond Lab is a software application for the analysis of 
# fixed income securities it provides a suite of applications
# in addition to standard fixed income analysis bond lab provides 
# for the specific analysis of structured products residential mortgage backed securities, 
# asset backed securities, and commerical mortgage backed securities
# License GPL3
# Copyright (C) 2014  Glenn M Schultz, CFA
# Fair use of the Bond Lab trademark is limited to promotion of the use of the software or 
# book "Investing in Mortgage Backed Securities Using Open Source Analytics" 

setMethod("initialize",
          signature("PrepaymentModelTune"),
          function(.Object,
                   TurnoverRate = "numeric",
                   Turnover.alpha = "numeric",
                   Turnover.beta = "numeric",
                   Turnover.theta = "numeric",
                   Seasonality.alpha = "numeric",
                   Seasonality.theta = "numeric",
                   Incentive.Fast.theta.1 = "numeric",
                   Incentive.Fast.theta.2 = "numeric",
                   Incentive.Fast.beta = "numeric",
                   Incentive.Fast.eta = "numeric",
                   Incentive.Slow.theta.1 = "numeric",
                   Incentive.Slow.theta.2 = "numeric",
                   Incentive.Slow.beta = "numeric",
                   Incentive.Slow.eta = "numeric",
                   Burnout.beta.1 = "numeric",
                   Burnout.beta.2 = "numeric"){
            
            .Object@TurnoverRate = TurnoverRate
            .Object@Turnover.alpha = Turnover.alpha
            .Object@Turnover.beta = Turnover.beta
            .Object@Turnover.theta = Turnover.theta
            .Object@Seasonality.alpha = Seasonality.alpha
            .Object@Seasonality.theta = Seasonality.theta
            .Object@Incentive.Fast.theta.1 = Incentive.Fast.theta.1
            .Object@Incentive.Fast.theta.2 = Incentive.Fast.theta.2
            .Object@Incentive.Fast.beta = Incentive.Fast.beta
            .Object@Incentive.Fast.eta = Incentive.Fast.eta
            .Object@Incentive.Slow.theta.1 = Incentive.Slow.theta.1
            .Object@Incentive.Slow.theta.2 = Incentive.Slow.theta.2
            .Object@Incentive.Slow.beta = Incentive.Slow.beta
            .Object@Incentive.Slow.eta = Incentive.Slow.eta
            .Object@Burnout.beta.1 = Burnout.beta.a
            .Object@Burnout.beta.2 = Burnout.beta.2
            
            return(.Object)
            callNextMethod(.Object,...)
          })