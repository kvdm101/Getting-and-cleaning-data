# You should create one R script called run_analysis.R that does the following. 
run_analysis <- 
    
#Download the file
    if(!file.exists("./data")){dir.create("./data")}
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
    
#Unzip the file
    unzip(zipfile="./data/Dataset.zip",exdir="./data")

#Folder for data is UCI HAR Dataset
    my_path <- file.path("./data" , "UCI HAR Dataset")
    files<-list.files(my_path, recursive=TRUE)
    #files

#Read data
   setwd("C:/Users/Karin-PC/Documents/Coursera/Data Science/3 Getting and cleaning data/UCI HAR Dataset/UCI HAR Dataset/train")
    RawTrain <- read.table("X_train.txt")
    RawTrainLabel <- read.table("y_train.txt")
    RawTrainSubjects <- read.table("subject_train.txt")
   setwd("C:/Users/Karin-PC/Documents/Coursera/Data Science/3 Getting and cleaning data/UCI HAR Dataset/UCI HAR Dataset/test")
    RawTest <- read.table("X_test.txt")
    RawTestLabel <- read.table("y_test.txt")
    RawTestSubjects <- read.table("subject_test.txt")
   setwd("C:/Users/Karin-PC/Documents/Coursera/Data Science/3 Getting and cleaning data/UCI HAR Dataset/UCI HAR Dataset/")
   my_path = getwd()  
   RawLabels <- read.table("features.txt")
    act_labels <- read.table("activity_labels.txt")
#features <- read.table("./UCI HAR Dataset/features.txt")[,2]

#initialise package dplyr that will mostly be used to tidy the data
library(dplyr)  

#get labels to use as column names in Test and Train
    temp <- t(RawLabels[,2])
    temp <- as.data.frame(temp)
    temp <- t(temp)

    TrainLabels <- merge(x = RawTrainLabel, y = act_labels, by = "V1", all.y=TRUE)
    TestLabels  <- merge(x = RawTestLabel, y = act_labels, by = "V1", all.y=TRUE)

#convert the raw data to tables and then overlay column names with labels
    Train <- tbl_df(RawTrain)
    colnames(Train) <- temp[,2:561]
    Test <- tbl_df(RawTest)
    colnames(Test) <- temp[,2:561]
    Train <- bind_cols(TrainLabels, Train)
    Test <- bind_cols(TestLabels, Test)

#Add subjects
    RawTrainSubjects <- rename(RawTrainSubjects, Subject = V1)
    Train <- bind_cols(Train, RawTrainSubjects)
    RawTestSubjects <- rename(RawTestSubjects, Subject = V1)
    Test <- bind_cols(Test, RawTestSubjects)

# Merges the training and the test sets to create one data set.
    myData <- bind_rows(Test, Train)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
    myData2 <- select(myData, 2, 480, contains("std()"), contains("mean()"))

# Appropriately labels the data set with descriptive variable names. 
    myData2 <- rename(myData2, Act_Desc = V2)
    names(myData2)<-gsub("^t", "time", names(myData2))
    names(myData2)<-gsub("^f", "frequency", names(myData2))
    names(myData2)<-gsub("Acc", "Accelerometer", names(myData2))
    names(myData2)<-gsub("Gyro", "Gyroscope", names(myData2))
    names(myData2)<-gsub("Mag", "Magnitude", names(myData2))
    names(myData2)<-gsub("BodyBody", "Body", names(myData2))
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(tidyr)
#Summarise data by activty and subject
    mySummary <- group_by(myData2, Act_Desc, Subject)
#get the mean/average for each variable
    myTidy <- mySummary %>% summarise_each(funs(mean))
#Save tidy data as a txt file
    write.table(myData2, file = "tidydata.txt",row.name=FALSE)

