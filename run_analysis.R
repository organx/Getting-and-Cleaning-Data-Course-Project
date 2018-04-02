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

# Goal is to output a clean and tidy dataset into a file named: tidy_file.txt
#
# Please Note: 
# All of the steps were done. They were done out of order and overlap.
# They are outlined below with boxed headings.
#
# Visual Overview of the Desired Merged Table Construction
#
# 
#        --------------------
#        |   features       |
#----------------------------
#| S |   |                  |
#| u |   |                  |
#| b |   |                  |
#| j |   |                  |
#| e | Y |                  |
#| c |   |                  |
#| t | T |    X Test        |
#|   | e |                  |
#| T | s |                  |
#| e | t |                  |
#| s |   |                  |
#| t |   |                  |
#|---------------------------
#| S |   |                  |
#| u |   |                  |
#| b |   |                  |
#| j |   |                  |
#| e | Y |                  |
#| c |   |                  |
#| t | T |   X Train        |
#|   | r |                  |
#| T | a |                  |
#| r | i |                  |
#| a | n |                  |
#| i |   |                  |
#| n |   |                  |
#|---------------------------
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
# Set the path of the source files relative to the working directory.
path <- "./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/"
#
# Read 'activity_labels.txt' into 'activities.'
# Paste was used to join filename and path.
# This was an effort to make the filename easier to read and follow.
# This import operation took two lines.
# This operation was condensed to one line for the next steps later in the script.
activitylabels <- paste(path, "activity_labels.txt", sep = "")
activities <- read.table(activitylabels, header = FALSE)

# Read 'features.txt' into 'features'.
# Upon reading the information surrounding the dataset, the creators did a good job.
# These names although they are not pretty, they are descriptive of the data.
# I will leave the column headers unchanged.
features <- read.table(paste(path,"features.txt", sep = ""), header = FALSE)

# Import 'subject_test' into R.
# Set the column name of data in 'subjecttest' to 'Subject'
subjecttest <- read.table(paste(path, "/test/subject_test.txt", sep = ""), col.names = "Subject")

# Import 'y_test' into 'ytest'.
ytest <- read.table(paste(path,"/test/y_test.txt", sep = ""), header = FALSE)

##################################################################
# Step 4: 
# Appropriately label the data set with descriptive variable names.
##################################################################
# Import 'X_test.txt' into 'xtest'. Use 'features.txt' to create column headers.
xtest <- read.table(paste(path,"/test/X_test.txt", sep = ""), col.names = features [,2])

 
##############################################################################
# Step #2
# Extracts only the measurements on the mean and standard deviation for each measurement.
##############################################################################
# Create parameters for finding columns that have 'mean()' and 'std()'
# Use 'grep()' to search and identify elements in 'features' that have 'mean()' and 'std()'
# The workhorse of this section are two commands:
# 1.) grep("mean()", features[,2])
# 2.) grep("std()", features[,2]) 
# The script will use these numbers to order the sets after parsing.
# This also works, but it outputs names: features$V2[grep("mean()", features$V2)].
# It would be useful if it can look for "mean()" and "str()" at the same time.

# This will search and identify rows for 'mean()' and then again for 'std()'
# Place resultant row numbers into a vector called 'subset.parameters'. 
# Sort the final 'subset.parameters' ascending.
subset.parameters <- grep("mean()", features[,2])
subset.parameters <- sort(c(subset.parameters, grep("std()", features[,2])))
sort(subset.parameters)

# Clear up some memory. Get ready for crunching numbers.
# Start carving the data set and remove the unwanted columns.
# Subset using 'subset parameters'
xtest <- xtest[,subset.parameters]

######################################################################
# Step 1: 
# Merges the training and the test sets to create one data set.
#######################################################################
# Add the respective activities codes from 'ytest' to left of 'xtest'.
# Subset the first column of 'ytest'.
# Name this new column 'Activity'.
Activity <- ytest[,1]
xtest <- cbind(Activity, xtest, deparse.level = 0)


# Combine 'x test' with 'subject test'
# Adds Subject/Participant data to 'xtest' with a column name of Subject
xtest <- cbind(subjecttest, xtest)

# 'xtest' complete!!!!!!!!!


#Now do the same for 'x train'. Note: It is a bigger file. It takes more memory.
# Import 'subject_train.txt' into R.
# Set the column name of data in 'subjecttrain' to 'Subject'
subjecttrain <- read.table(paste(path, "/train/subject_train.txt", sep = ""), col.names = "Subject")


# Importing 'y_train' into 'ytrain'
ytrain <- read.table(paste(path,"/train/y_train.txt", sep = ""), header = FALSE)

# Import 'X_train.txt' into 'xtrain'. Use 'features.txt' to create column headers.
xtrain <- read.table(paste(path,"/train/X_train.txt", sep = ""), col.names = features [,2])



# Clear up some memory. Get ready for crunching numbers.
# Start carving the data set and remove the unwanted columns.
# Subset using 'subset parameters'
xtrain <- xtrain[,subset.parameters]

# Add the respective activities codes from 'ytrain' to left of 'xtrain'.
# Subset the first column of 'ytrain'.
# Name this new column 'Activity'.
Activity <- ytrain[,1]
xtrain <- cbind(Activity, xtrain, deparse.level = 0)

# Combine 'xtrain' with 'subjecttest'
# Add Subject/Participant data to 'xtrain' with a column name of 'Subject'
xtrain <- cbind(subjecttrain, xtrain)

#####################################################################
# Finish Step #1
# Merge the training and the test sets to create one data set.
#####################################################################

# Finally bind the bottom to the top.
# Combine rawtrain to the bottom of rawtest
tidyset <- rbind(xtest,xtrain)


########################################################################
# Step #3
# Uses descriptive activity names to name the activities in the data set
########################################################################
# Substitute the numbers for neat activity labels.
# I'm pulling out my hair trying to use 'gsub' inside a function.
# I'll just have to brute force it with some nasty subsetting hacks inside a loop.
# The print statement is for debugging purposes.

# Create a neat reference table without factors.
act <- as.character(activities$V2)

# Create a backup dataframe with which to hack.
# It wastes a bit of memory but it makes up in redo time.
# Besides, I like parsing by number so let's keep both data sets.
tidyset2 <- tidyset

# Begin hack.
for (i in 1:6){
        tidyset2$Activity[tidyset2[,2] == i] <- act[i]
        print (levels(activities$V2)[i])
}

# Check the work
unique(tidyset2$Activity)

# Activity codes added. End hack.


####################################################
# Step #5
# From the data set in step 4, create a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
#
# In short: group and summarize 'tidyset' using dplyr.
####################################################
library(dplyr)

# Create a third copy. Arrange its rows by 'Subject'.
tidyset3 <- arrange(tidyset2, Subject)

tidy_final <- tidyset3 %>%
        group_by(Subject, Activity) %>%
        summarize_all(funs(mean))
                    
write.table(tidy_final,"tidy_file.txt", row.name = FALSE)
                       
# End script
###########################
