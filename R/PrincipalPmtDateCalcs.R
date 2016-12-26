
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

  # These functions calcualte the difference between dates
  # using the lubridate package 

  setGeneric("AddMonths", function(Date = "character", Months = numeric())
    {standardGeneric("AddMonths")})

  #' A function to add months to a date
  #'
  #' @param Date A character string the date to which the number of months are added
  #' @param Months A numeric value the number of months to add to Date
  #' @importFrom lubridate %m+%
  #' @examples AddMonths("03-01-2015", 360)
  #' @export
  AddMonths <- function(Date = "character", Months = numeric()){
  as.Date(Date, format = "%m-%d-%Y") %m+% months(Months)
  }

  setGeneric("SubtractMonths", function(Date = "character", Months = numeric())
    {standardGeneric("SubtractMonths")})

  #' A function to subtract months to a date
  #'
  #' @param Date A character string the date to which the number of months 
  #' are subtracted
  #' @param Months A numeric value the number of months to subtract from Date
  #' @importFrom lubridate %m-%
  #' @examples SubtractMonths("03-01-2015", 360)
  #' @export
  SubtractMonths <- function(Date = "character", Months = numeric()){
    as.Date(Date, format = "%m-%d-%Y") %m-% months(Months)
  }

  #' A function to report the first and last principal payment dates
  #'
  #' @param date.vector A character string referencing the payment date slot of
  #' class PassThrough.
  #' @param principal.vector A character string referencing a principal payment slot
  #' @param type A character string the payment date either "FirstPmt" or "LastPmt"
  #' @export
  PrincipalPmtDate <- function(date.vector = "character",
                               principal.vector = "character",
                               type = "character"){

    if(!type %in% c("FirstPmt", "LastPmt")) stop("Not a valid type")

    firstpmt <- function(x){x!=0}
    lastdate <- length(principal.vector)

    PaymentIndex <- date.vector[which(sapply(principal.vector, firstpmt ) == TRUE)[1]]

    if(type == "FirstPmt") {unname(PaymentIndex)
    }else{unname(date.vector[lastdate])}}



