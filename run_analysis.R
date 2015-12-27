rm(list=ls())

# set working directory
setwd("/Users/arnejol/Documents/DataScience/GettingAndCleaningData/assignment")
getwd()

#-----------------------------------------------
# Step 0: download the data
#-----------------------------------------------
#date <- "27-12-2015"
#fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileURL,"./data/sensordara.zip", mode="wb")
#unzip("./data/sensordara.zip")

#-----------------------------------------------
# Step 1: Merge train and test dataset
#-----------------------------------------------
d_X_train <- read.table("./UCI HAR Dataset/train/X_train.txt",header = FALSE)
d_Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt",header = FALSE)
colnames(d_Y_train) <- c("class_num")
d_subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",header = FALSE)
d_train <- cbind(d_subject_train, d_X_train, d_Y_train)

# test data
d_X_test <- read.table("./UCI HAR Dataset/test/X_test.txt",header = FALSE)
d_Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt",header = FALSE)
colnames(d_Y_test) <- c("class_num")
d_subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",header = FALSE)
d_test <- cbind(d_subject_test, d_X_test, d_Y_test)

# merge train and test data
d_total <- rbind(d_train, d_test)

# remove unused dataframes
rm(d_subject_test, d_subject_train, d_test, d_train, d_X_test, d_X_train, d_Y_test, d_Y_train)

#-----------------------------------------------
# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
#-----------------------------------------------

# put features names in a vector
features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, colClasses = c("integer","character"))
# create meaningfull names
colnames(features) <-  c("col_num","feature_name")

# get the index of the features that match to mean() or std()
index <- grep("mean\\(\\)|std\\(\\)",features$feature_name)
# add 1 to index vector and add 1 as first element and 563 as last element in order
# to select proper values from d_total dataframe
index_tot <- c(1, index+1, 563)

# select only the measurements of mean and sd, and the first and last row.
d_mean_sd <- d_total[,index_tot]       

#-----------------------------------------------
# Step 3: Add descriptive activity names to name the activities in the data set
#-----------------------------------------------

# join activity labels
# read acivity labels from text file
d_activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE)
# add meaningfull collnames
colnames(d_activity_labels) <- c("number","description")
# merge activity labels to the dataframe
d_mean_sd_2 <- merge(d_mean_sd, d_activity_labels, by.x="class_num", by.y="number", all.x= TRUE)

#-----------------------------------------------
# Step 4: Appropriately labels the data set with descriptive variable names
#-----------------------------------------------

# add descriptive variable names
# variable names are in features, and index indicates the selected features. Add class_num, subject_id 
# in from of the features and activity lable at the end to match the order in de d_mean_sd_2 dataframe
colnames(d_mean_sd_2) <- c("class_num", "subject_id", features[index,2], "activity_lable")

#-----------------------------------------------
# Step 5: From the data set in step 4, creates a second, independent tidy 
#         data set with the average of each variable for each activity
#         and each subject.
#-----------------------------------------------

# 5.a calculate the mean by subject+activity_lable
require(reshape2)
d_mean_sd_3 <- melt(d_mean_sd_2, id.vars=c("subject_id", "activity_lable"))

## 5.b. Now can recast the data applying mean as a function.
tidy_data_mean <- dcast(d_mean_sd_3, subject_id + activity_lable ~ variable, mean)

# write datafile i .txt format
write.table(tidy_data_mean,file="./tidy_data_mean.txt", row.name=FALSE)
