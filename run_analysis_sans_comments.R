########################################################
# Class Project
# Ted Herring
# Coursera, Getting and Cleaning Data, Week 4
# by Johns Hopkins, Bloomberg School of Public Health
# 
# Data source: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# INTRODUCTION:

# Raw data was obtained from a study that had 30 different participants (subjects) 
# wear a Galaxy brand smartphone on their waist while they performed 6 different activities.
# The data was collected and put into a datasets.
#
# Goal is to output a clean and tidy dataset into a file named: tidy_file.txt
#
# Please Note: 
# All of the steps were done. They were done out of order and overlap.
# They are outlined below with boxed headings.
#
#Summary:
# The top (xtest) is created by putting together 4 datasets:
#       1. putting together xtest, and features
#       2. adding ytest
#       3. adding subject_test
#
# The bottom part (ytest) is created by putting together 3 datasets.
#       1. skip adding features to the top
#       2. Put together xtrain and ytrain
#       3. Add subjecttest
#
# When this is done, the bottom will be bound to the top.
# --------------------------------------------------------
#
# --------------------------------------------------------
# START SCRIPT
#######################################################################
# Get Data and Read the Data
#######################################################################
path <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"
activitylabels <- paste(path, "activity_labels.txt", sep = "")
activities <- read.table(activitylabels, header = FALSE)
features <- read.table(paste(path,"features.txt", sep = ""), header = FALSE)
subjecttest <- read.table(paste(path, "/test/subject_test.txt", sep = ""), col.names = "Subject")
ytest <- read.table(paste(path,"/test/y_test.txt", sep = ""), header = FALSE)



##################################################################
# Step 4: 
# Appropriately label the data set with descriptive variable names.
##################################################################
xtest <- read.table(paste(path,"/test/X_test.txt", sep = ""), col.names = features [,2])


 
##############################################################################
# Step #2
# Extracts only the measurements on the mean and standard deviation for each measurement.
##############################################################################
subset.parameters <- grep("mean()", features[,2])
subset.parameters <- sort(c(subset.parameters, grep("std()", features[,2])))
sort(subset.parameters)

xtest <- xtest[,subset.parameters]



######################################################################
# Step 1: 
# Merges the training and the test sets to create one data set.
#######################################################################
Activity <- ytest[,1]
xtest <- cbind(Activity, xtest, deparse.level = 0)
xtest <- cbind(subjecttest, xtest)

subjecttrain <- read.table(paste(path, "/train/subject_train.txt", sep = ""), col.names = "Subject")
ytrain <- read.table(paste(path,"/train/y_train.txt", sep = ""), header = FALSE)
xtrain <- read.table(paste(path,"/train/X_train.txt", sep = ""), col.names = features [,2])

xtrain <- xtrain[,subset.parameters]
Activity <- ytrain[,1]

xtrain <- cbind(Activity, xtrain, deparse.level = 0)
xtrain <- cbind(subjecttrain, xtrain)



#####################################################################
# Finish Step #1
# Merge the training and the test sets to create one data set.
#####################################################################
tidyset <- rbind(xtest,xtrain)



########################################################################
# Step #3
# Uses descriptive activity names to name the activities in the data set
########################################################################
act <- as.character(activities$V2)

tidyset2 <- tidyset

for (i in 1:6){
        tidyset2$Activity[tidyset2[,2] == i] <- act[i]
        print (levels(activities$V2)[i])
}

# Check the work
unique(tidyset2$Activity)



####################################################
# Step #5
# From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
#
# In short: group and summarize 'tidyset' using dplyr.
####################################################
library(dplyr)

tidyset3 <- arrange(tidyset2, Subject)

tidy_final <- tidyset3 %>%
        group_by(Subject, Activity) %>%
        summarize_all(funs(mean))
                    
write.table(tidy_final,"tidy_file.txt", row.name = FALSE)
                       


# End script
###########################
