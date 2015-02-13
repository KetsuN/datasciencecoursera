## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
	correlationVector <- c()

	##  Determine all file names in the specified directory (ending in .csv)
	file.names <- list.files(directory, full.names = TRUE)
	
	##  Iterate through the file names
	for (fileName in file.names) {			
		##  Read in the file
		fileFrame <- read.csv(fileName)
		
		##  Determine the file/monitor ID
		fileId <- as.numeric(fileFrame[1, 4])
		
		##  Iterate through all records in the file
		completeCount <- 0
		numRows <- nrow(fileFrame)
		
		for (i in 1:numRows) {
			##  Add 1 to number observations if nitrate and sulfate values are present
			if (!is.na(fileFrame [i, 2]) && !is.na(fileFrame [i, 3])) {
				completeCount <- completeCount + 1
			}
		}
		
		if (completeCount > threshold) {
			correlationVector <- c(correlationVector, cor(fileFrame[2], fileFrame[3], use="pairwise.complete.obs", method="pearson"))
		}
	}
	
	##  Provide column names to the data frame and ensure the listing matches the order of the id parameter
	correlationVector
}