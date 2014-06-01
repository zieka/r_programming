#
# rankhospital.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
# Function returns a character vector with the name of the
# hospital that has the ranking speciﬁed by the num argument
#


rankhospital <- function(state, outcome, num = "best") {
  #Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  suppressWarnings(data[, c(3, 4, 5)] <- sapply(data[, c(3, 4, 5)], as.numeric))
  data <- data[data$State == state, ]

  #Check that state and outcome are valid
  valid_states <- unique(data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

  if (!(state %in% valid_states)){
    stop("invalid state")
  }
  if (!(outcome %in% valid_outcomes)){
    stop("invalid outcome")
  }

  if (outcome == "heart attack"){
    data <- data[order(data[, 3], data[, 1]), ]
    data <- data[!is.na(data[, 3]), ]
  }
  else if (outcome == "heart failure"){
    data <- data[order(data[, 4], data[, 1]), ]
    data <- data[!is.na(data[, 4]), ]
  }
  else if (outcome == "pneumonia"){
    data <- data[order(data[, 5], data[, 1]), ]
    data <- data[!is.na(data[, 5]), ]
  }

  #Return hospital name in that state with the given rank 30-day death rate
  if (num=="best"){
    data[1,1]
  }
  else if (num=="worst"){
    data[nrow(data), 1]
  }
  else if (!(is.numeric(num))){
    return("invalid num")
  }
  else if (as.numeric(num) > nrow(data)){
    return(NA)
  }
  else if (as.numeric(num)){
    data[as.numeric(num), 1]
  }
}