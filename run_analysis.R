
# Merges the training and the test sets to create one data set.

trainx <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
testx <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
xTable <- rbind(trainx, testx)
trainsubject <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
testsubject <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
subjectTable <- rbind(trainsubject, testsubject)
trainy <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
testy <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
yTable <- rbind(trainy, testy)

# Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
groupFeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
newTable <- xTable[, groupFeatures]
names(newTable) <- features[groupFeatures, 2]
names(newTable) <- gsub("\\(|\\)", "", names(newTable))
names(newTable) <- tolower(names(newTable))

# Uses descriptive activity names to name the activities in the data set.

activities <- read.table("C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
yTable[,1] = activities[yTable[,1], 2]
names(yTable) <- "Activity"

# Appropriately labels the data set with descriptive variable names.

names(subjectTable) <- "Subject"
cleanData <- cbind(subjectTable, yTable, xTable)
write.table(cleanData, "C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/cleanData.txt")

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
cleanDataMeans <- ddply(cleanData, .(Subject, Activity), limitedColMeans)
names(cleanDataMeans)[-c(1,2)] <- paste0("Mean", names(cleanDataMeans)[-c(1,2)])
write.table(cleanDataMeans, "C:/Users/Mitch/Documents/DataScience/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/cleanDataAverage.txt", row.names = FALSE)