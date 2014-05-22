#
# corr.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
# Function takes a directory of data files and a threshold for
# complete cases and calculates the correlation between sulfate and
# nitrate for monitor locations where the number of completely
# observed cases (on all variables) is greater than the threshold
#

corr <- function(directory, threshold = 0) {

  result_v <- NULL

  for (i in 1:332) {
    file_name <- sprintf("%.03d.csv", i)
    file_path <- paste(directory, file_name, sep="/")
    data <- read.csv(file_path, header=TRUE)
    data <- na.omit(data)
    nobs <- nrow(data)

    if (nobs > threshold) {
      sulfate <- data[,"sulfate"]
      nitrate <- data[,"nitrate"]
      new_cor <- cor(sulfate,nitrate)
      result_v <- c(result_v, new_cor)
    }
  }

  print(result_v)
}
