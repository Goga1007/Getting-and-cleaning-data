# Getting-and-cleaning-data
## 5 steps of getting and cleaning data for peer graded assignment 
# Download the data set
#Data set downloaded and extracted under the folder called getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset

library(data.table)
## reading the tables from downloaded datasets and saving them under new names for merging 
featureNames <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
activityLabels <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", header = FALSE)
#reading train data
subjectTrain <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", header = FALSE)
#reading test data
subjectTest <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("C:/Users/gogab/Documents/R Studio files/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", header = FALSE)

## 1. Merges the training and the test sets to create one data set.
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

##nameing the columns from metadata info
colnames(features) <- t(featureNames[2])

## merging data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
## head(completeData) to check if the data is uploaded and merged for further analysis

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
## adding two new columns after the last one with mean and std
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
## extract selected data
extractedData <- completeData[,requiredColumns]
dim(extractedData)

## 3. Uses descriptive activity names to name the activities in the data set
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
## Variable names made as factors
extractedData$Activity <- as.factor(extractedData$Activity)
#descriptive variable names
names(extractedData)

## 4.Appropriately labels the data set with descriptive variable names. 
## changing the variable names to be more descriptive and understandable
names(extractedData) <- gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData) <- gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData) <- gsub("BodyBody", "Body", names(extractedData))
names(extractedData) <- gsub("Mag", "Magnitude", names(extractedData))
names(extractedData) <- gsub("^t", "Time", names(extractedData))
names(extractedData) <- gsub("^f", "Frequency", names(extractedData))
names(extractedData) <- gsub("tBody", "TimeBody", names(extractedData))
names(extractedData) <- gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("angle", "Angle", names(extractedData))
names(extractedData) <- gsub("gravity", "Gravity", names(extractedData))
#The results
names(extractedData)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# setting the subject as a factor variable
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
# Tidy Data for average on each activity and subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "tidyData.txt", row.names = FALSE)
# export tidyData
str(tidyData)
tidyData
