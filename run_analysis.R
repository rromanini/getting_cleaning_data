#################################################################################
#Task: Getting and Cleaning Data Course Project
#
#Author: Rui Romanini
#Date: 02/26/2016
#################################################################################

#Import librarys
library(dplyr)

#################################################################################
#1.Merges the training and the test sets to create one data set.
# Read file x_train.txt
xtrain = read.csv("./data/UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="")

# Read file y_train.txt
ytrain = read.csv("./data/UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="")

# Read file y_train.txt
subjecttrain = read.csv("./data/UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="")

# Read file x_test.txt
xtest = read.csv("./data/UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="")

# Read file y_test.txt
ytest = read.csv("./data/UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="")

# Read file y_subject.txt
subjecttest = read.csv("./data/UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="")

# create a unique dataset for test
test = data.frame(subjecttest,xtest,ytest)

# create a unique dataset for train
train = data.frame(subjecttrain,xtrain,ytrain)

#Merge test ande train datasets
test_train_dataset = rbind(test,train)

# Remove not used variables
rm(xtest,ytest,xtrain,ytrain,subjecttest,subjecttrain)


###########################################################################################
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#select column names
features <- read.csv("./data/UCI HAR Dataset/features.txt", sep="", header=FALSE)
columns_names <- as.vector(features[, 2])
## remove paratheses from feature names 
columns_names<-gsub("\\()","",columns_names)  
##replace initial f with "freq"
columns_names<-gsub("^f","freq-",columns_names)  
##replace initial t with "time"
columns_names<-gsub("^t","time-",columns_names)  
## removes illegal characters and makes colnames unique 
columns_names<-make.names(columns_names, unique =T, allow_ = TRUE) 

colnames(test_train_dataset) <- c("subject_id", "activity_labels", columns_names)

run_data <- select(test_train_dataset, contains("subject"), contains("label"), 
                                           contains("mean"), contains("std"), -contains("freq"), 
                                           -contains("angle")) 

####################################################################################################
#3.Uses descriptive activity names to name the activities in the data set
activity_labels <- read.csv("./data/UCI HAR Dataset/activity_labels.txt",  
                                                        sep="", header=FALSE) 

###################################################################################################
#4.Appropriately labels the data set with descriptive variable names. 
run_data$activity_labels <- as.character(activity_labels[ 
     match(run_data$activity_labels, activity_labels$V1), 'V2']) 

###############################################################################################################
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
run_average <- run_data %>% 
     group_by(subject_id, activity_labels) %>% 
     summarise_each(funs(mean)) 

#####################################################################################################
# save run_data_summary.txt 
write.table(run_average, file="run_data_summary.txt", row.name=FALSE) 

