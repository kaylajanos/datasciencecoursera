setwd("C:/Users/569375/Desktop/DSTB/Clean Data/project")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","Smart.zip")
#unzipping data
unzip(zipfile="smart.zip",exdir="./data")
#Drilling into folder we want
setwd("data/UCI HAR Dataset")
list.files()


#Reading in the Feature Names
features<-read.table("features.txt")
features.names<-features[,2] #only need second column as names
mean.std<-grepl("mean|std",features.names) #Identifying mean/std related variables
summary(mean.std) #79 Trues indicates we should be keeping 79 activities from each x dataset 
rm(features) #Removing features since we have extracted the names already

#Reading in Activity Labels 
activity<-read.table("activity_labels.txt")
names(activity)<- c("Act_Num","Activity")

########################
#      TEST DATA       #
########################

#Reading in Test data
test_x<-read.table("test/x_test.txt",header=FALSE)
#Adding variable names to text_x
names(test_x)<-features.names
names(test_x)

#Only Keeping Mean/STD Related variables 
test_x<-test_x[,mean.std] #Using list of names from earlier to identiy variables should be kept
  #79 Variables remain, thats what we wanted.

#Reading in column lables for test 
test_y<-read.table("test/y_test.txt")
names(test_y)<-"Act_Num"  #Adding correct variable name for so that activity can be merged later

#Reading in Subjects 
subject_test<-read.table("test/subject_test.txt")
names(subject_test)<-"Subject" #Updating variable 

#Cbinding all three datasets onto each other 
test1<-cbind(subject_test,test_y,test_x)
test1$Act_Num

#Merging activity name on (cant be done before now, reorders dataset)
test<-merge(test1,activity,by=("Act_Num"))

#Removing all extra datasets 
rm(subject_test,test1,test_x,test_y)


########################
#    TRAINING DATA     #
########################

#Reading in the training data
train_x<-read.table("train/x_train.txt",header=FALSE)
#Adding Variable Names 
names(train_x)<-features.names


#Subsetting for only mean and std variables 
train_x<-train_x[,mean.std]
  #79 remain thats what we wanted

#Reading in the activity labels 
train_y<-read.table("train/y_train.txt")
names(train_y)<-"Act_Num" #Updating name for later merge 

#Reading in Subjects
subjects_train<-read.table("train/subject_train.txt")
names(subjects_train)<-"Subject"

#Cbinding all together 
train1<-cbind(subjects_train,train_y,train_x)

#Merging on Activity Name
train<-merge(train1,activity,by=("Act_Num"))

#Removing extra datasets
rm(subjects_train,train1,train_y,train_x,activity,mean.std,features.names)


#######################
#    Full Dataset     #
#######################

#Rbinding together
data<-rbind(test,train)

#Removing Act_Num since its been replaced with activity
data<-data[,(2:82)]
names(data)
#Fixing all names to be tidier
names(data)<-gsub("^t", "Time ", names(data))
names(data)<-gsub("^f", "Frequency ", names(data))
names(data)<-gsub("Acc", "Accelerometer ", names(data))
names(data)<-gsub("Gyro", "Gyroscope ", names(data))
names(data)<-gsub("Mag", "Magnitude ", names(data))
names(data)<-gsub("BodyBody", "Body ", names(data))
names(data)<-gsub("([()])","",names(data))
names(data)<-gsub("meanFreq","Mean Frequency",names(data))
names(data)<-gsub("mean","Mean",names(data))
names(data)<-gsub("std","Standard Deviation",names(data))




#Looking at new names
names(data)



#Step 5, mean for each variable by subject
#using melt to group by activity and subject 
data$Subject<-as.factor(data$Subject)
levels(data$Subject)
levels(data$Activity)

data_agg<-aggregate(.~Subject+Activity,data,mean)
data_agg
tidyData <- data_agg[order(data_agg$Subject,data_agg$Activity),]
tidyData

write.table(tidyData, row.names = FALSE, "tidyData.txt")
