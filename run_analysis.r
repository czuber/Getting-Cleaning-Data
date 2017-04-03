# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

######################
#   Requirement 1    #
######################
# Set working directory
setwd("C:/Users/czuber/Downloads/Getting_Cleaning_Data")
# URL to Database Download
data.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# Download Database Zip File
download.file(data.url, destfile = "Getting_Cleaning_Assignment_Data.zip", mode = "wb")
# Unzip Database File
unzip(zipfile = "Getting_Cleaning_Assignment_Data.zip", exdir = "./data")

# Read in Training Tables
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
# Reading testing tables:
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
# Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')
# Reading activity labels:
activity_labels <- read.table('./data/UCI HAR Dataset/activity_labels.txt')

#########################
#   Requirements 2-4    #
#########################
# Assign Activity Names with Descriptive Variable Names
colnames(x_train) <- features[,2]
colnames(y_train) <-"ActivityID"
colnames(subject_train) <- "SubjectID"
colnames(x_test) <- features[,2] 
colnames(y_test) <- "ActivityID"
colnames(subject_test) <- "SubjectID"
colnames(activity_labels) <- c('ActivityID','ActivityType')

# Bind Each Table Together For Test & Train Data
# Stack Final Test & Final Train Tables Toegther
train <- cbind(y_train, subject_train, x_train)
test <- cbind(y_test, subject_test, x_test)
data <- rbind(train, test)

# List of Column Names
data.column.names <- colnames(data)

# Subset Columns Based on Whether THe name Contains "Mean" or "STD"
column.names.mean.std <- (grepl("ActivityID" , data.column.names) | 
                   grepl("SubjectID" , data.column.names) | 
                   grepl("mean.." , data.column.names) | 
                   grepl("std.." , data.column.names) )

data.mean.std <- data[ , column.names.mean.std == TRUE]

# Clean Up Column Names with Abbreviations
names(data.mean.std) <- gsub("BodyBody", "Body", names(data.mean.std))
names(data.mean.std) <- gsub('Acc',"Accelerometer",names(data.mean.std))
names(data.mean.std) <- gsub('GyroJerk',"AngularAcceleration",names(data.mean.std))
names(data.mean.std) <- gsub('Gyro',"Gyroscope",names(data.mean.std))
names(data.mean.std) <- gsub('Mag',"Magnitude",names(data.mean.std))
names(data.mean.std) <- gsub('^t',"Time",names(data.mean.std))
names(data.mean.std) <- gsub('^f',"Frequency",names(data.mean.std))
names(data.mean.std) <- gsub('mean()',".Mean",names(data.mean.std))
names(data.mean.std) <- gsub('std()',".StandardDeviation",names(data.mean.std))
names(data.mean.std) <- gsub('Freq\\.',"Frequency.",names(data.mean.std))
names(data.mean.std) <- gsub('Freq$',"Frequency",names(data.mean.std))
names(data.mean.std) <- gsub("[][!#$%()*,.:;<=>@^_`|~.{}]","",names(data.mean.std))
# names(data.mean.std)

######################
#   Requirement 5    #
######################
require(dplyr)
# Join Activity Labels |
# Remove ActivityID |
# Group By Subject ID & ActivityID |
# Remove NAs |
# Calculate Mean For All Columns |
# Order By SubjectID & ActivityID

data.tidy <- left_join(data.mean.std, activity_labels, by = "ActivityID") %>%
  select(-ActivityID) %>%
  group_by(SubjectID, ActivityType) %>%
  na.omit() %>%
  summarize_each(funs(mean(., na.rm = T))) %>%
  arrange(SubjectID, ActivityType)

# Write Data To Files
write.table(data.tidy, "TidyDataSet_With_Averages.txt", row.name=FALSE)
write.csv(data.tidy, "TidyDataSet_With_Averages.csv", row.names = F)