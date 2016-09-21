#Getting and Cleaning Data Project
#Requirements
##
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
filename <- "getdata_dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename)
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}
#=============================================================================
# STEP ZERO: Prepare environment
#=============================================================================
if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}

if (!require("plyr")) {
  install.packages("plyr")
}

require("data.table")
require("reshape2")
require("plyr")#=============================================================================
# STEP ONE: Merges the training and the test sets to create one data set.
#=============================================================================
# Read Test Data Files
testSubject <- read.table('./UCI HAR Dataset/test/subject_test.txt')
testX <- read.table('./UCI HAR Dataset/test/x_test.txt')
testY <- read.table('./UCI HAR Dataset/test/y_test.txt')

# create a set column for DF
set <- 'test'
#merge first two dt's and identify set. using cbind so rows remain in original sort
testSY <- cbind(testSubject,set,testY)
#merge third dt's
test <- cbind(testSY,testX)

# Read Train Data Files
trainSubject <- read.table('./UCI HAR Dataset/train/subject_train.txt')
trainX <- read.table('./UCI HAR Dataset/train/x_train.txt')
trainY <- read.table('./UCI HAR Dataset/train/y_train.txt')

# create a set column for DF
set <- 'train'
#merge first two dt's and identify set. using cbind so rows remain in original sort
trainSY <- cbind(trainSubject,set,trainY)
#merge third dt's
train<- cbind(trainSY,trainX)

#merge the test and train data tables
c3pd <- rbind(test,train)
# clean up the environment
rm(test,testSubject,testY,testX,testSY,train,trainSubject,trainY,trainX,trainSY)
#=============================================================================
# STEP TWO: Extracts only the measurements on the mean and standard deviation 
#           for each measurement.
#=============================================================================

#read feature column  names
colNames <- read.table('./UCI HAR Dataset/features.txt',stringsAsFactors=FALSE)[[2]]

colNames <- c("subject","set","activity", colNames)

#set remaining column names from features.txt
names(c3pd) <- colNames

# discard data columns without "mean' or "std"
colNames <- names(c3pd)
selectedCols <- c("subject" ,"set", "activity", grep("-(mean|std)\\(\\)", colNames, value=TRUE))
c3pd <- c3pd[,selectedCols]

#=============================================================================
# STEP THREE: Uses descriptive activity names to name the activities in the data set
#=============================================================================
# replace value in the activity column with their text equivelents from activity_labels.txt
c3pd$activity[c3pd$activity==1] <- "walking"
c3pd$activity[c3pd$activity==2] <- "walking upstairs"
c3pd$activity[c3pd$activity==3] <- "walking downstairs"
c3pd$activity[c3pd$activity==4] <- "sitting"
c3pd$activity[c3pd$activity==5] <- "standing"
c3pd$activity[c3pd$activity==6] <- "laying"

#=============================================================================
# STEP FOUR Appropriately labels the data set with descriptive activity names.
#=============================================================================

# translate column names
colNames <- names(c3pd)
colNames <- gsub(pattern="^t",replacement="time",x=colNames)
colNames <- gsub(pattern="^f",replacement="freq",x=colNames)
colNames <- gsub(pattern="-?mean[(][)]-?",replacement="Mean",x=colNames)
colNames <- gsub(pattern="-?std[()][)]-?",replacement="Std",x=colNames)
colNames <- gsub(pattern="-?meanFreq[()][)]-?",replacement="MeanFreq",x=colNames)
colNames <- gsub(pattern="BodyBody",replacement="Body",x=colNames) 
names(c3pd) <- colNames

#=============================================================================
# STEP FIVE: Creates a second, independent tidy data set with the average of 
#            each variable for each activity and each subject.
#=============================================================================

# calc average mean and export tidy data set

id_labels   = c("subject","set","activity")
data_labels = setdiff(colnames(c3pd), id_labels)
melt_data      = melt(c3pd, id = id_labels, measure.vars = data_labels)

tidy_data   = dcast(melt_data, subject + activity ~ variable, mean)
write.table(tidy_data, file = "./tidy_data.txt",row.name=FALSE, sep = "\t")
