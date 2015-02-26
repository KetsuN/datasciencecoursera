##  Function name:  Best
##
##  Description:  This function takes a two character state code and an
##  	outcome name (heart attack, heart failure, or pneumonia).  The function
##		outputs the the hospital name for the hospital with the lowest mortality
##		rate for the specified outcome in the specified state
##
##	Author: Tym "KetsuN" Butler

best <- function(state, outcome) {
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
	
	## Check that state and outcome are valid
	if (state %in% unique(fileData[, stateCol])) {
		stateData <- fileData[which(fileData[, stateCol] == state), ]
	
		if (outcome == "heart attack") {
			## Get hospital names in that state with lowest 30-day death for heart attacks
			stateData[, heartAttackCol] <- sapply(stateData[, heartAttackCol], as.character)
			stateData[, heartAttackCol] <- sapply(stateData[, heartAttackCol], as.numeric)
			lowestDeath <- stateData[which(stateData[, heartAttackCol] == min(stateData[, heartAttackCol], na.rm=TRUE)), ][, hospitalCol]
		} else if (outcome == "heart failure") {
			## Get hospital names in that state with lowest 30-day death for heart failure
			stateData[, heartFailureCol] <- sapply(stateData[, heartFailureCol], as.character)
			stateData[, heartFailureCol] <- sapply(stateData[, heartFailureCol], as.numeric)
			lowestDeath <- stateData[which(stateData[, heartFailureCol] == min(stateData[, heartFailureCol], na.rm=TRUE)), ][, hospitalCol]
		} else if (outcome == "pneumonia") {
			## Get hospital names in that state with lowest 30-day death for heart pneumonia
			stateData[, pneumoniaCol] <- sapply(stateData[, pneumoniaCol], as.character)
			stateData[, pneumoniaCol] <- sapply(stateData[, pneumoniaCol], as.numeric)
			lowestDeath <- stateData[which(stateData[, pneumoniaCol] == min(stateData[, pneumoniaCol], na.rm=TRUE)), ][, hospitalCol]
		} else {
			stop("invalid outcome")
		}
		
		## Return first abbreviation
		bestHospital <- sort(lowestDeath)[1]
		bestHospital
		
	} else {
		stop("invalid state")
	}	
}