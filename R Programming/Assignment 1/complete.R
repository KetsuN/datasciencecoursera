## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {
	completeFrame <- as.data.frame(x= numeric(0), y= integer(0), z = character(0))

	##  Determine all file names in the specified directory (ending in .csv)
	file.names <- list.files(directory, full.names = TRUE)
	
	##  Iterate through the file names
	for (fileName in file.names) {			
		##  Read in the file
		fileFrame <- read.csv(fileName)
		
		##  Determine the file/monitor ID
		fileId <- as.numeric(fileFrame[1, 4])
		
		##  Determine if the ID of the record matches a specified ID
		if (fileId %in% id) {		
			##  ID matches, iterate through all records in the file
			completeCount <- 0
			numRows <- nrow(fileFrame)
			
			for (i in 1:numRows) {
				##  Add 1 to number observations if nitrate and sulfate values are present
				if (!is.na(fileFrame [i, 2]) && !is.na(fileFrame [i, 3])) {
					completeCount <- completeCount + 1
				}
			}
			
			##  Add a row to the resulting data frame
			completeFrame <- rbind(completeFrame, c(fileId, completeCount))
		}
	}
	
	##  Provide column names to the data frame and ensure the listing matches the order of the id parameter
	names(completeFrame) <- c("id", "nobs")
	completeFrame[match(id, completeFrame$"id"),]
}