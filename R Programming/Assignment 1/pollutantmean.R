## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

pollutantmean <- function(directory, pollutant, id = 1:332) {
	pollutantVector <- c ()

	##  Determine all file names in the specified directory (ending in .csv)
	file.names <- list.files(directory, full.names = TRUE)
	
	##  Iterate through the file names
	for (fileName in file.names) {			
		##  Read in the file
		fileFrame <- read.csv(fileName)
		
		##  Determine if the ID of the record matches a specified ID
		fileId <- as.numeric(fileFrame[1, 4])
		if (fileId %in% id) {
			##  ID matches
			##  Add the pollutant type of the record to the pollutant vector
			if (pollutant == "sulfate") {
				pollutantVector <- c(pollutantVector, fileFrame[, c("sulfate")])
			} else if (pollutant == "nitrate") {
				pollutantVector <- c(pollutantVector, fileFrame[, c("nitrate")])
			}
		}
	}
	
	mean(pollutantVector, na.rm = TRUE)
}