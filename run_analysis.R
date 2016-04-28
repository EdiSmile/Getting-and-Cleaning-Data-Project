###Install required packages if you haven't done yet
install.packages('dplyr')
install.packages('data.table')
install.packages('tidyr')
###Load required functions
library(dplyr)
library(data.table)
library(tidyr)

#checked your own path
filesPath <- "C:/Users/WeiNa/Documents/Data Science Certifcate/Data Science Specialization/Getting and Cleaning Data/Assignment/UCI HAR Dataset"
# Please notice two major data sets: training and test
# Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))
# Read activity label files
dataActTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))
#Read data files
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))
#** you can check the six data sets on the Global Enviroment



### Q1 Merges the training and the test sets to create one data set
# Merges the training and the test sets to create one data set 
# for both Activity and Subject files this will merge the training and the test sets by row binding 
#and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataAct<- rbind(dataActTrain, dataActTest)
setnames(alldataAct, "V1", "activityNum")

# Merge the training and test files, named DataAll
DataAll <- rbind(dataTrain, dataTest)

# name variables according to feature 
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(DataAll) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataAct)
DataAll <- cbind(alldataSubjAct, DataAll)
#** You can check the consolidated table on 'DataAll' table


########################################################
### Q2 Extracts only the measurements on the mean and standard deviation for each measurement
# Extracting the mean and standard deviation in "features.txt"
dataAllMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

# Adding the measurements for the mean and standard deviation with "subject","activityNum" 
# into the consoldiated table
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataAllMeanStd)
DataAll<- subset(DataAll,select=dataFeaturesMeanStd) 
#** You can check again the consolidated table on 'DataAll' table


##################################################
### Q3 Uses descriptive activity names to name the activities in the data set
##enter name of activity into dataTable
DataAll <- merge(activityLabels, DataAll , by="activityNum", all.x=TRUE)
DataAll$activityName <- as.character(DataAll$activityName)

## create a table with variable means by subject and activity's order
DataAll$activityName <- as.character(DataAll$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = DataAll, mean) 
DataAll<- tbl_df(arrange(dataAggr,subject,activityName))


######################################################
### Q4 Labels the data set with descriptive variable names.
#leading t or f is based on time or frequency measurements.
#Body = related to body movement.
#Gravity = acceleration of gravity
#Acc = accelerometer measurement
#Gyro = gyroscopic measurements
#Jerk = sudden movement acceleration
#Mag = magnitude of movement
#mean and SD are calculated for each subject for each activity for 
#each mean and SD measurements. The units given are g's for the 
#accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec 
#for the corresponding jerks.

head(str(DataAll),2)

# Results as below
## Classes 'tbl_df', 'tbl' and 'data.frame':    180 obs. of  69 variables:
##  $ subject                    : int  1 1 1 1 1 1 2 2 2 2 ...
##  $ activityName               : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
##  $ activityNum                : num  6 4 5 1 3 2 6 4 5 1 ...
##  $ tBodyAcc-mean()-X          : num  0.222 0.261 0.279 0.277 0.289 ...
##  $ tBodyAcc-mean()-Y          : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
##  $ tBodyAcc-mean()-Z          : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
## .......

names(DataAll)<-gsub("std()", "SD", names(DataAll))
names(DataAll)<-gsub("mean()", "MEAN", names(DataAll))
names(DataAll)<-gsub("^t", "time", names(DataAll))
names(DataAll)<-gsub("^f", "frequency", names(DataAll))
names(DataAll)<-gsub("Acc", "Accelerometer", names(DataAll))
names(DataAll)<-gsub("Gyro", "Gyroscope", names(DataAll))
names(DataAll)<-gsub("Mag", "Magnitude", names(DataAll))
names(DataAll)<-gsub("BodyBody", "Body", names(DataAll))
# Names after
head(str(DataAll),6)

print(DataAll)

## Results shown as below
## Classes 'tbl_df', 'tbl' and 'data.frame':    180 obs. of  69 variables:
##  $ subject                                       : int  1 1 1 1 1 1 2 2 2 2 ...
##  $ activityName                                  : chr  "LAYING" "SITTING" "STANDING" "WALKING" ...
##  $ activityNum                                   : num  6 4 5 1 3 2 6 4 5 1 ...
##  $ timeBodyAccelerometer-MEAN()-X                : num  0.222 0.261 0.279 0.277 0.289 ...
##  $ timeBodyAccelerometer-MEAN()-Y                : num  -0.04051 -0.00131 -0.01614 -0.01738 -0.00992 ...
##  $ timeBodyAccelerometer-MEAN()-Z                : num  -0.113 -0.105 -0.111 -0.111 -0.108 ...
##  $ timeBodyAccelerometer-SD()-X                  : num  -0.928 -0.977 -0.996 -0.284 0.03 ...
##  $ timeBodyAccelerometer-SD()-Y                  : num  -0.8368 -0.9226 -0.9732 0.1145 -0.0319 ...
##  .......

########################################################

### Q5 From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject
Data2<-aggregate(. ~subject + activityName, DataAll, mean)
Data2<-Data2[order(Data2$subject,Data2$activityName),]
write.table(Data2, file = "./tidydata.txt",row.name=FALSE)
### You can find a copy under your c:/user/"your user"/docuements
