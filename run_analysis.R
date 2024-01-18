# run_analysis.R script

temp <- tempfile() #creates a temp empty file

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp) #downloads the zip into the temp file

unzip(temp, exdir="./data") #unzip the data

#Reads all data 

testvalues <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
testlabels <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
testsubjects <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

trainvalues <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
trainlabels <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
trainsubjects <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")


## Merges the training and the test datasets

alltest <- cbind(testlabels, testsubjects, testvalues) #binds test datasets
alltrain <- cbind(trainlabels, trainsubjects, trainvalues) #binds train datasets

mergeset <- rbind(alltrain, alltest) #binds train and test

features <-  read.table("./data/UCI HAR Dataset/features.txt",header = FALSE) #reads feature table

colnames(mergeset)[3:563] <- features [ ,2] #renames all the feature columns according to the feature.txt data

colnames(mergeset)[1]<- "activityid" #renames first column
colnames(mergeset)[2]<- "subjects" #renames second column
colnames(mergeset) #checks the names of the columns


## Extracts only the measurements on the mean and standard deviation for each measurement. 

colNames <- colnames(mergeset) #stores the column names for easier access

colNames

means <- grep ("mean()", colNames) #takes the columns with mean
sds <- grep ("std()", colNames) # takesthe columns with sd (standard deviation)

meansd <- cbind (mergeset["activityid"], mergeset["subjects"],  mergeset[means], mergeset[sds]) #dataframe only with means and sd

## Uses descriptive activity names to name the activities in the data set

activityLabels <-  read.table("./data/UCI HAR Dataset/activity_labels.txt", header = FALSE, stringsAsFactors = T)

colnames(activityLabels) <- c("activityid", "activitytype")

activitytype <- sub("_", " ", activityLabels$activitytype) #removes underscore from the activity names

activityLabels[,2] <- activitytype #replaces the activitytype column. Now the names are without underscore.

descriptive <- merge(meansd, activityLabels, by = "activityid", all.x = TRUE)

library(dplyr)

descriptive <- select(descriptive, last_col(), everything()) #moves the last column to the first position

activitytype <- strsplit(descriptive$activitytype, "_") #removes underscore from the names of the column

categories <- as.factor(unique(descriptive$activitytype)) # to check if the names are correct
categories


# Appropriately labels the data set with descriptive variable names.


colnames(descriptive) <- tolower(names(descriptive)) # lowercase for column names


# From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.


tidy <- descriptive %>% 
  group_by(subjects, activitytype) %>%
  summarise_all(list(mean))

tidy <- tidy[, c(3,2,1, 4:82)] # reorder the columns to look better


write.table(tidy, "tidydata.txt", row.name=FALSE) #saves the data