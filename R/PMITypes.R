

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
  
  #' An S4 class representing PMITypes
  #' 
  #' This class is used to calculate and pass PMITypes reported to the investor
  #' and used in cashflow analytics.  For example, PMI is reported to the investor
  #' as a numeric decimal or decimal string but basis is used to determime the PMI
  #' amount paid to the insurer by the homeowner.
  #' @slot PMIDecimal a numeric value the PMI fee expressed as decimal numeric
  #' @slot PMIBasis a numeric value the PMI free expresed as basis in decimal 
  #' numeric form
  #' @slot PMIDecimalString a character string the PMI expressed in decimal 
  #' form.
  #' @exportClass PMITypes
  setClass("PMITypes",
           representation(
             PMIDecimal = "numeric",
             PMIBasis = "numeric",
             PMIDecimalString = "character"
           ))
  setGeneric("PMITypes", function(object)
    {standardGeneric("PMITypes")})
