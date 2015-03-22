library(reshape2)

# Helpers
readingNames <- read.table("UCI HAR Dataset\\features.txt")[, 2]
activityNames <- read.table("UCI HAR Dataset\\activity_labels.txt")[, 2]

# Training Data
rawData <- read.table("UCI HAR Dataset\\train\\X_train.txt")
colnames(rawData) <- readingNames
subjectData <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
activityData <- read.table("UCI HAR Dataset\\train\\y_train.txt")
mergeData <- cbind(subjectData, activityData, rawData)

# Testing Data
rawData <- read.table("UCI HAR Dataset\\test\\X_test.txt")
colnames(rawData) <- readingNames
subjectData <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
activityData <- read.table("UCI HAR Dataset\\test\\y_test.txt")

# Merge training and test data together
mergeData <- rbind(mergeData, cbind(subjectData, activityData, rawData))
colnames(mergeData)[1] <- "Subject"
colnames(mergeData)[2] <- "Activity"

# Parse only mean and std readings
meanData <- mergeData[c("Subject", "Activity", grep("mean|std", colnames(mergeData), value = TRUE))]

# Add activity labels
meanData[,2] <- replace(meanData[,2], meanData[,2]==1, "WALKING")
meanData[,2] <- replace(meanData[,2], meanData[,2]==2, "WALKING_UPSTAIRS")
meanData[,2] <- replace(meanData[,2], meanData[,2]==3, "WALKING_DOWNSTAIRS")
meanData[,2] <- replace(meanData[,2], meanData[,2]==4, "SITTING")
meanData[,2] <- replace(meanData[,2], meanData[,2]==5, "STANDING")
meanData[,2] <- replace(meanData[,2], meanData[,2]==6, "LAYING")

# Modify column labels to be more "meaningful"
# I have chosen to not change the names since the variable names
# are meaning ful as they are.

# Transform into summary table
meanMelt <- melt(meanData, id = c("Subject", "Activity"), measure.vars = grep("mean|std", colnames(mergeData), value = TRUE))
meanMelt <- dcast(meanMelt, Subject + Activity ~ variable,mean)
write.table(meanMelt, file = "finalData.txt", sep=' \t', row.name=FALSE)
