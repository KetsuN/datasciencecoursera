colMean <- function(x, removeNa = TRUE) {
	numCols <- ncol(x)
	means <- numeric(numCols)
	for (i in 1:numCols) {
		means[i] <- mean(x[,i], na.rm = removeNa)
	}
	means
}