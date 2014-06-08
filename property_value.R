#
# property_value.R
#
# Copyright (C) 2014 Kyle Scully
#
# Author(s)/Maintainer(s):
# Kyle Scully
#
#	Downloads American Community Survey and subsets section on property value
# Function property_value_frequency take the threshold for property values
# as defined in the American Community Survey code book:
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
#
# Property value
# bb .N/A (GQ/rental unit/vacant, not for sale only)
# 	01 .Less than $ 10000
# 	02 .$ 10000 - $ 14999
# 	03 .$ 15000 - $ 19999
# 	04 .$ 20000 - $ 24999
# 	05 .$ 25000 - $ 29999
# 	06 .$ 30000 - $ 34999
# 	07 .$ 35000 - $ 39999
# 	08 .$ 40000 - $ 49999
# 	09 .$ 50000 - $ 59999
# 	10 .$ 60000 - $ 69999
# 	11 .$ 70000 - $ 79999
# 	12 .$ 80000 - $ 89999
# 	13 .$ 90000 - $ 99999
# 	14 .$100000 - $124999
# 	15 .$125000 - $149999
# 	16 .$150000 - $174999
# 	17 .$175000 - $199999
# 	18 .$200000 - $249999
# 	19 .$250000 - $299999
# 	20 .$300000 - $399999
# 	21 .$400000 - $499999
# 	22 .$500000 - $749999
# 	23 .$750000 - $999999
# 	24 .$1000000+


#Download file if it doesnt already exist in working directory
if (!file.exists("./american_community_survey.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", destfile="./american_community_survey.csv", method="curl")
}

property_value_frequency <- function(threshold_value){

  #Verify threshold_value is in range
  if ( threshold_value < 1 | threshold_value > 24){
    stop("threshold value is out of range (1-24)")
  }


	data <- read.csv("./american_community_survey.csv", header=T)

  #subset data and build as a data frame
  property_value <- data[,"VAL"]
  df <- as.data.frame(table(property_value))

  #return frequency value
  df[df$property_value==threshold_value, "Freq"]
}