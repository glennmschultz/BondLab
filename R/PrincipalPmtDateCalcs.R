
  # Bond Lab is a software application for the analysis of 
  # fixed income securities it provides a suite of applications
  # mortgage backed, asset backed securities, and commerical mortgage backed 
  # securities Copyright (C) 2018  Bond Lab Technologies, Inc.


  # These functions calcualte the difference between dates
  # using the lubridate package 


  #' @title Principal payment dates
  #' @family ScenarioAnalysis
  #' @description In the case of mortgage and asset backed securities the 
  #' first and last principal payment date are of interest to the investors. 
  #' This funnction computes the first principal payment and last principal 
  #' payment dates.
  #' @param Date A character string the date to which the number of months are added
  #' @param Months A numeric value the number of months to add to Date
  #' @importFrom lubridate %m+%
  #' @examples AddMonths("03-01-2015", 360)
  #' @export
  AddMonths <- function(Date = "character", Months = numeric()){
  as.Date(Date, format = "%m-%d-%Y") %m+% months(Months)
  }


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



