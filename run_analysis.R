## File run_analysis.R
## Data: Collected from accelerometers from Samsung Galaxy S smartphone
## Purpose: Collect the data, clean it, write the result to tidy_data.txt
## For more details see the README.md file

library(dplyr)

###############################################################################
#
# Prep. 1: Download the data zip file and unzip it
#
###############################################################################

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
dataPath <- "UCI HAR Dataset"

if(!file.exists(zipFile)) download.file(zipUrl, zipFile, mode = "wb")
if (!file.exists(dataPath)) unzip(zipFile)

###############################################################################
#
# Prep. 2: Read the data
#
###############################################################################

## Read the features list (without converting the text labels to factors)
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

## Read the activity labels, and assign meaningful headers
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

## Read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testData <- read.table(file.path(dataPath, "test", "X_test.txt"))
testLabels <- read.table(file.path(dataPath, "test", "y_test.txt"))

## Read train data
trainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainData <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainLabels <- read.table(file.path(dataPath, "train", "y_train.txt"))

###############################################################################
#
# Step 1: Merge the training and the test sets to create one data set
#
###############################################################################

## Merge Tables
allActivities <- rbind(
  cbind(testSubjects, testData, testLabels),
  cbind(trainSubjects, trainData, trainLabels)
)

## Remove temp variables (separately read files)
rm(testSubjects, testData, testLabels,
   trainSubjects, trainData, trainLabels)

## Assign headers (colnames)
colnames(allActivities) <- c("subject", features[,2], "activity")

###############################################################################
#
# Step 2: Extract only the measurements on the mean
#         and standard deviation for each measurement
#
###############################################################################

## Use grepl to determine which columns to keep
neededColumns <- grepl("subject|mean|std|activity", colnames(allActivities))

## Use the "neededColumns" list to update the table
allActivities <- allActivities[,neededColumns]

###############################################################################
#
# Step 3: Use descriptive activity names to name the activities in the data set
#
###############################################################################

## Change the activity values with a factor containing activity names
allActivities$activity <- factor(allActivities$activity,
                                 levels = activities[,1],
                                 labels = activities[,2])

###############################################################################
#
# Step 4: Appropriately label the data set with descriptive variable names
#
###############################################################################

## Put colnames in a temp variable
allActivitiesColNames <- colnames(allActivities)

## Remove special characters
allActivitiesColNames <- gsub("[\\(\\)-]", "", allActivitiesColNames)

## Expand abbreviations and fix typos
allActivitiesColNames <- gsub("^f", "FrequencyDomain", allActivitiesColNames)
allActivitiesColNames <- gsub("^t", "TimeDomain", allActivitiesColNames)
allActivitiesColNames <- gsub("mean", "Mean", allActivitiesColNames)
allActivitiesColNames <- gsub("std", "StandardDeviation", allActivitiesColNames)
allActivitiesColNames <- gsub("Freq", "Frequency", allActivitiesColNames)
allActivitiesColNames <- gsub("Mag", "Magnitude", allActivitiesColNames)
allActivitiesColNames <- gsub("Acc", "Accelerator", allActivitiesColNames)
allActivitiesColNames <- gsub("Gyro", "Gyroscope", allActivitiesColNames)
allActivitiesColNames <- gsub("BodyBody", "Body", allActivitiesColNames)

## Use corrected names as the new colnames
colnames(allActivities) <- allActivitiesColNames

###############################################################################
#
# Step 5: From the data set in step 4,
#         create a second, independent tidy data set
#         with the average of each variable for each activity and each subject
#
###############################################################################

## Create the new data set with means of all activities columns
allActivitiesMeans <- allActivities %>%
                      group_by(subject, activity) %>%
                      summarise_all(funs(mean))

## Write to file: tidy_data.txt
write.table(allActivitiesMeans, "tidy_data.txt",
            row.names = FALSE, quote = FALSE)