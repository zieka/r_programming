#
# best.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
# The function should check the validity of its arguments.
# If an invalid state value is passed to best, the function
# should throw an error via the stop function with the exact
# message “invalid state”. If an invalid outcome value is
# passed to best, the function should throw an error via the
# stop function with the exact message “invalid outcome”.
#

best <- function(state, outcome) {
  #Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #Check that state and outcome are valid
  valid_states <- unique(data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

  if (!(state %in% valid_states)){
    stop("invalid state")
  }
  if (!(outcome %in% valid_outcomes)){
    stop("invalid outcome")
  }

  data <- data[data$State == state, ]
  suppressWarnings(data[, c(11, 17, 23)] <- sapply(data[, c(11, 17, 23)], as.numeric))

  #Order the data
  data <- data[order(data[, 2]), ]

  #Return hospital name in that state with lowest 30-day death rate
  if (outcome == "heart attack"){
    data[which.min(data[, 11]), "Hospital.Name"]
  }
  else if (outcome == "heart failure"){
    data[which.min(data[, 17]), "Hospital.Name"]
  }
  else if (outcome=="pneumonia"){
    data[which.min(data[, 23]), "Hospital.Name"]
  }
}
