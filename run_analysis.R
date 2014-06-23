library(reshape2)

# set wd to temporary working folder
folder <- "C:/Users/Guillermo/Desktop 2"

if(!file.exists(paste(folder, "/download", sep="")))
	dir.create(paste(folder, "/download", sep=""))
	
setwd(paste(folder, "/download", sep=""))

# download file and unzip-it
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "./dataset.zip")
unzip("./dataset.zip")

# procesaFiles ()
# process training & test files, adding id's & labels
# observationsFile: file with all the observations
# activityFile: file with the activities IDs
# subjectsFile: file with the person's IDs

procesaFiles <- function(path, observationsFile, activityFile, subjectsFile) {
	# read files
	wdirectory <- paste(folder, path, sep="")
	oFile <- read.table(paste(wdirectory, observationsFile, sep=""), header=F)
	aFile <- read.table(paste(wdirectory, activityFile, sep=""), header=F)
	sFile <- read.table(paste(wdirectory, subjectsFile, sep=""), header=F)

	# properly set the variable names
	features <- read.table(paste(wdirectory, "/../features.txt", sep=""), header=F)	
	colnames(oFile) <- features[,2]
	
	# eliminate unwanted columns
	good<- grep("-mean\\(\\)|-std\\(\\)", features[,2])
	oF <- subset(oFile, select = good)
	
	# add activity & subjects IDs
	fileWIDs <- cbind(oF, aFile, sFile)
	colnames(fileWIDs)[67] <- "activityID"
	colnames(fileWIDs)[68] <- "subjectID"
	
	# join fileWIDs with activity labels
	activityLabels <- read.table(paste(wdirectory, "/../activity_labels.txt", sep=""), header=F)
	fileWLables <- merge(fileWIDs, activityLabels, by.x="activityID", by.y= "V1")
	colnames(fileWLables)[69] <- "activity"
	
	fileWLables
}

training <- procesaFiles("/UCI HAR Dataset/train", "/X_train.txt", "/Y_train.txt", "/subject_train.txt")
test <- procesaFiles("/UCI HAR Dataset/test", "/X_test.txt", "/Y_test.txt", "/subject_test.txt")

# merge both files
dataSet <- rbind(training, test)

# calculate means
ids <- c("subjectID", "activity", "activityID")
dataNames <- setdiff(colnames(dataSet), ids)
melt_data <- melt(dataSet, id = ids, measure.vars = dataNames)
cleanDataSet <- dcast(melt_data, subjectID + activity ~ variable, mean)

# create output file
write.table(cleanDataSet, "./cleanData.txt")