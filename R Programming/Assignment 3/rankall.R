##  Function name:  rankall
##
##  Description:  This function takes a two character state code and an
##  	outcome name (heart attack, heart failure, or pneumonia).  The function
##		outputs the the hospital name for the hospital with the lowest mortality
##		rate for the specified outcome in the specified state
##
##	Author: Tym "KetsuN" Butler

rankall <- function(outcome, num = "best") {
	## Read outcome data
	fileData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospitalCol <- 2
	stateCol <- 7
	heartAttackCol <- 11
	heartFailureCol <- 17
	pneumoniaCol <- 23
	
	## Ranking matrix to contain results
	rankingList <- data.frame(hospital=character(), state=character(), stringsAsFactors = FALSE)
	##print(str(rankingList))
	##rankingList <- as.data.frame(x = character(0), y = character(0), z = numeric(0))
	
	## Convert the text string o fit the data
	outcome <- tolower(outcome)
	if (typeof(num) == "character") {
		num <- tolower(num)
	}
	
	## Check that state and outcome are valid
	for (state in unique(sort(fileData[, stateCol]))) {
		stateData <- fileData[which(fileData[, stateCol] == state), ]
	
		if (outcome == "heart attack") {
			## Rank hospitals in the state based on heart attack rates (ties decided by hospital name)
			stateData[, heartAttackCol] <- sapply(stateData[, heartAttackCol], as.character)
			stateData[, heartAttackCol] <- sapply(stateData[, heartAttackCol], as.numeric)
			orderedStats <- stateData[which(!is.na(stateData[, heartAttackCol])), ]
			orderedStats <- orderedStats[order(orderedStats[,heartAttackCol], orderedStats[,hospitalCol]), ][, hospitalCol]
		} else if (outcome == "heart failure") {
			## Rank hospitals in the state based on heart failure rates (ties decided by hospital name)
			stateData[, heartFailureCol] <- sapply(stateData[, heartFailureCol], as.character)
			stateData[, heartFailureCol] <- sapply(stateData[, heartFailureCol], as.numeric)
			orderedStats <- stateData[which(!is.na(stateData[, heartFailureCol])), ]
			orderedStats <- orderedStats[order(orderedStats[,heartFailureCol], orderedStats[,hospitalCol]), ][, hospitalCol]
		} else if (outcome == "pneumonia") {
			## Rank hospitals in the state based on pneumonia rates (ties decided by hospital name)
			stateData[, pneumoniaCol] <- sapply(stateData[, pneumoniaCol], as.character)
			stateData[, pneumoniaCol] <- sapply(stateData[, pneumoniaCol], as.numeric)
			orderedStats <- stateData[which(!is.na(stateData[, pneumoniaCol])), ]
			orderedStats <- orderedStats[order(orderedStats[,pneumoniaCol], orderedStats[,hospitalCol]), ][, hospitalCol]
		} else {
			stop("invalid outcome")
		}
		
		if ((typeof(num) == "double") & (num <= length(orderedStats))) {
			rankedHospital <- orderedStats[num]
		} else if ((typeof(num) == "double") & (num > length(orderedStats))){
			rankedHospital <- NA
		} else if ((typeof(num) == "character") & (num == "best")) {
			rankedHospital <- orderedStats[1]
		} else if ((typeof(num) == "character") & (num == "worst")) {
			rankedHospital <- orderedStats[length(orderedStats)]
		} else {
			stop("invalid num")
		}
		
		rankingList[nrow(rankingList)+1, ] <- c(as.character(rankedHospital), as.character(state))
	} 
	##  Return ranking results	
	rankingList
}