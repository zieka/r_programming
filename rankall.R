#
# rankall.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
# Function returns a 2-column data frame containing the
# hospital in each state that has the ranking speciﬁed (num)
# for a specified outcome (outcome)
#

source("rankhospital.R")

rankall <- function(outcome, num = "best") {
  #Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #Check that state and outcome are valid
  valid_states <- unique(data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")

  if (!(outcome %in% valid_outcomes)){
    stop("invalid outcome")
  }

  #initialize dataframe for result
  hospitals <- data.frame()

  #For each state, find the hospital of the given rank
  for (state in valid_states){
    new_row <- data.frame(hospital = rankhospital(state, outcome, num),state = state)
    rownames(new_row) <- new_row$state
    hospitals <- rbind(hospitals,new_row)
  }

  #Return a data frame with the hospital names and the state name
  hospitals[order(as.character(hospitals$state)),]

}
