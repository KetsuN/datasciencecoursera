##  Function name:  rankhospital
##
##  Description:  This function takes a two character state code, an
##  	outcome name (heart attack, heart failure, or pneumonia), and
##		a ranking qualifier (numeric, "best", or "worst".  The function
##		outputs the the hospital name for the hospital with the "best", 
##		"worst", or matching num rank for mortality rates for the
##		specified outcome in the specified state.
##
##	Author: Tym "KetsuN" Butler

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	fileData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hospitalCol <- 2
	stateCol <- 7
	heartAttackCol <- 11
	heartFailureCol <- 17
	pneumoniaCol <- 23
	
	## Convert the text string o fit the data
	state <- toupper(state)
	outcome <- tolower(outcome)
	if (typeof(num) == "character") {
		num <- tolower(num)
	}
	
	## Check that state and outcome are valid
	if (state %in% unique(fileData[, stateCol])) {
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
		
		##  Find the correct rank for in the hospital name vector based on the num argument
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
		
		## Return first abbreviation
		rankedHospital
		
	} else {
		stop("invalid state")
	}	
}