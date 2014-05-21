#
# complete.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
# Function reads a directory full of files and reports the
# number of completely observed cases in each data file
#

complete <- function(directory, id = 1:332) {

	result_df <- data.frame()

	for (i in id) {
		file_name <- sprintf("%.03d.csv", i)
		file_path <- paste(directory, file_name, sep="/")
		data <- read.csv(file_path, header=TRUE)
		data <- na.omit(data)
		nobs <- nrow(data)
		new_row <- c(i,nobs)
		result_df <- rbind(result_df,new_row)

	}

	colnames(result_df) <- c("id", "nobs")
	print(result_df)
}