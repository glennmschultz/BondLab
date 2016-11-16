
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
  
  #' An S4 class representing the borrower Gwac borrower (note rate)
  #' 
  #' This class is used to create and pass GWac (note rate) Types reported
  #' to the investor and used in the cash flow analytics.  For example, GWac
  #' is reported to investors as numeric decimal or numeric decimal string but
  #' the basis is used to determine the interest paid to the trust.
  #' @slot GWacDecimal A numeric value the GWac (borrower note rate)
  #' expressed as numeric decimal
  #' @slot GWacBasis A numeric value the GWac expressed in basis notation e.g.
  #' 0.055
  #' @slot GWacDecimalString A character value the GWac (borrower note rate)
  #' expressed as numeric decimal notation e.g. "5.50
  #' @exportClass GWacTypes
  setClass("GWacTypes",
           representation(
             GWacDecimal = "numeric",
             GWacBasis = "numeric",
             GWacDecimalString = "character")
           )
  setGeneric("GWacTypes", function(GWac)
    {standardGeneric("GWacTypes")})