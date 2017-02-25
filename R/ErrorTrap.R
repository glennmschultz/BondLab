

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

  #--------------------------
  # Error trap function for bond inputs
  #--------------------------  
  ErrorTrap <- function(bond.id = "character", 
                        principal = numeric(), 
                        settlement.date = "character", 
                        price = "character") {
  #Error inputs by the user make sure all needed dates and values are 
  #passed to Bond Cash Flow
  issue.date = bond.id@IssueDate
  start.date = bond.id@DatedDate
  end.date = bond.id@Maturity
  lastpmt.date = bond.id@LastPmtDate
  nextpmt.date = bond.id@NextPmtDate
  coupon = bond.id@Coupon
  frequency = bond.id@Frequency
  
  # Test Bond Inputs
  if(missing(issue.date)) stop("missing issue date")
  if(missing(start.date)) stop("missing start date")
  if(missing (end.date)) stop("missing end (Maturity) date")
  if(missing(lastpmt.date)) stop("missing last payment date")
  if(missing (nextpmt.date)) stop("missing next payment date")
  if(missing (coupon)) stop("missing coupon")
  if(missing (frequency)) stop("missing frequency")
  #Test Trade Inputs
  if (missing (settlement.date)) stop("missing settlement date")
  if(missing (principal)) stop("missing principal")
  if(missing (price)) stop("missing price")
  
  issue.date = as.Date(c(issue.date), "%m-%d-%Y")
  start.date = as.Date(c(start.date), "%m-%d-%Y")
  end.date = as.Date(c(end.date), "%m-%d-%Y")
  settlement.date = as.Date(c(settlement.date), "%m-%d-%Y")
  lastpmt.date = as.Date(c(lastpmt.date), "%m-%d-%Y")
  nextpmt.date = as.Date(c(nextpmt.date), "%m-%d-%Y")
  
  #============= Pass dates into the function m-d-y ======================
  
  #Error Trapping on Dates settlement date < next payment date or 
  #settlement date , issue date    
  if(settlement.date > nextpmt.date) 
    stop ("Settlement Date is Greater Than Next Payment Date")
  if(settlement.date < issue.date) stop ("Settlement Date is Less Than Issue Date")
  if(settlement.date < lastpmt.date) 
    stop ("Settlement Date is Less Than Last Payment Date")
   
  #Error Trap the user's price input
  if(price <= 0) stop("No valid bond price")
  #Error Trap the user's coupon input
  if (coupon < 0 | coupon > 100) stop("No valid coupon specified.")
  #Note: Minimum demonination needs to be added to class bond information 
  #as well as function input
  }
