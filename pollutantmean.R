pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  data_set <- NULL
  
	for (i in id) {
		file_name <- sprintf("%.03d.csv", i)
		file_path <- paste(directory, file_name, sep="/")
		data <- read.csv(file_path, header=TRUE)
    data_set <- rbind(data_set, data)    
	}
  
  subset_data <- data_set[,pollutant]
	print(mean(subset_data, na.rm = TRUE))
}
