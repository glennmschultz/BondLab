  
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
  
  #' A S4 class representing yield to maturity
  #' 
  #' This class is used to create and pass YieldTypes reported by Bond Lab 
  #' analytics and reported to investors.  It does not calculate yield to maturity
  #' rather it holds the value in decimal number, basis, and decimal string form.
  #' @slot YieldDecimal a numeric value the yield to maturity in decimal number form
  #' @slot YieldBasis a numeric value the yield to maturity in basis form
  #' @slot YieldDecimalString a character string the yield to maturity
  #' @exportClass YieldTypes
  setClass("YieldTypes",
    representation(
      YieldDecimal = "numeric",
      YieldBasis = "numeric",
      YieldDecimalString = "character")
    )
  
  setGeneric("YieldTypes", function(yield)
    {standardGeneric("YieldTypes")})
