library(plyr)
library(reshape)

setwd("~/Documents//GitHub_Repos//datasciencecoursera//gettingAndCleaningData")

## This is a script to read, clean and tidy accelometer data from the fillowing source
##https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

##the following link has a more complete description of the data and its sources
##http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones   

#First read in the data matrix column names Features.txt

Activity<-read.table("UCI HAR Dataset//activity_labels.txt", stringsAsFactors = FALSE)
Features<-read.table("UCI HAR Dataset//features.txt", stringsAsFactors = FALSE)

#Second read in Test data set files, this includes the filtered accelometer data, subject and activity mappings
Test.Data<-read.table("UCI HAR Dataset//test//X_test.txt", header=FALSE, stringsAsFactors = FALSE)
Test.Activity<-read.table("UCI HAR Dataset//test//y_test.txt", stringsAsFactors = TRUE)
Test.Subjects<-read.table("UCI HAR Dataset//test//subject_test.txt", stringsAsFactors = TRUE)

#Second read in Train data set files, this includes the filtered accelometer data, subject and activity mappings
Train.Data<-read.table("UCI HAR Dataset//train//X_train.txt", header=FALSE, stringsAsFactors = FALSE)
Train.Activity<-read.table("UCI HAR Dataset//train//y_train.txt", stringsAsFactors = TRUE)
Train.Subjects<-read.table("UCI HAR Dataset//train//subject_train.txt", stringsAsFactors = TRUE)

#Once all data is read into memory, all files were combined to create a large 2D table
#of the test data and the train data. The two complete datasets are then combined by binding rows.

Test.Complete<-cbind(Test.Subjects,Test.Activity,Test.Data)
colnames(Test.Complete)<-c(c("Subj", "Activity"),Features$V2)

Train.Complete<-cbind(Train.Subjects,Train.Activity,Train.Data)
colnames(Train.Complete)<-c(c("Subj", "Activity"),Features$V2)

Data.Comp<-rbind(Test.Complete,Train.Complete)

#Rename Activity factor levels to Descriptive Activity names. First convert Subj and Activity
#columns to factors

Data.Comp$Subj<-factor(Data.Comp$Subj)
Data.Comp$Activity<-factor(Data.Comp$Activity)

Data.Comp$Activity<-mapvalues(Data.Comp$Activity ,from = 1:6, to = Activity$V2)

#Taking a quick look of the data resultant data frame to make sure everything looks correct

head(Data.Comp)
summary(Data.Comp)
str(Data.Comp)
any(is.na(Data.Comp))

#Now only the mean and Std Dev. were extracted for the Accelerometer and Gyroscopic
#measurements, all calculated values were omitted (e.g. tBodyAccJerk, tBodyGravity, fBody, etc)

trimmed.Data<-Data.Comp[, grep("^tBody(Acc|Gyro)-mean\\(\\)-", colnames(Data.Comp))]
trimmed.Data<-cbind(trimmed.Data, Data.Comp[,grep("^tBody(Acc|Gyro)-std\\(\\)", colnames(Data.Comp))])
trimmed.Data<-cbind(Data.Comp[,1:2], trimmed.Data)

colnames(trimmed.Data)
summary(trimmed.Data)

#Now we "melt"/"reshape" the data to a long and skinny format (first normal-esque form)
trimm.data.2<-reshape(trimmed.Data, varying=colnames(trimmed.Data[3:14]), 
              v.names="Mean", timevar="Measurement", direction = "long",
              times=colnames(trimmed.Data[3:14]))

#remove the rownames and use sequential index instead
rownames(trimm.data.2)<-seq_along(trimm.data.2$id)

#create a array of the means of values grouped by measurement, subject and activity
SumData<-tapply(trimm.data.2$Mean, 
       list(trimm.data.2$Subj,trimm.data.2$Activity, as.factor(trimm.data.2$Measurement)), mean)

#melt the ugly array into a tidy dataset (long and skinny again)
cleanData<-melt.array(SumData, varnames=names(dimnames(SumData)))

#rewrite column names to be more informative
colnames(cleanData)<-c("Subj","Activity", "Measurement", "Mean")

#check that file looks alright
head(cleanData)

#output file
write.table(cleanData,file = "cleanData.txt",sep = "\t", col.names = TRUE, row.names=FALSE)
