library(dplyr) # used for inner-join 

## Create a new directory and move there

dir.create("RunAnalysis")
setwd("RunAnalysis")

## Getting the .Zip

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"data.zip",method="curl")
unzip("data.zip")
setwd("UCI HAR Dataset")

## Loading the features file

features <- read.table("features.txt",header = F, colClasses = "character")$V2
# we select the features "standard deviation" and "mean"
featuresIndex<-grep("(std\\(\\))|mean\\(\\)",features)
# and improve a bit the naming convention
features<-gsub("\\(\\)","",features)
features<-gsub("-",".",features)
features<-make.names(features)

## Loading the Label->Activity table

activityLabels<-read.table("activity_labels.txt",header = F, colClasses = "character",col.names = c("ActivityLabel","Activity"))

## This function replaces the ActivityLabel by the Activity proper name
## and returns a 1-column dataframe
renameActivity<-function(activity=data.frame()){
  # inner_join preserves the oder only in the *first* dataframe
  inner_join(activity,activityLabels, by="ActivityLabel")[,"Activity",drop=FALSE]
}

## This function reads the three files: "subject_*.txt" "X_*.txt" and "y_*.txt"
## (i.e., the subject, measurement and activity files)
## and assembles the three dataframes into one big dataframe
## We only select the colums of "X_*.txt" that matches "std()" or "mean()"
## The output is a data.frame with headers:
## [Subject Index] - [Activity's Name] - 66x[Feature's Name]
readDataFolder<-function(folder=character()){
  #Reading subject file
  subject <-read.table(file.path(folder,gsub("NAME",folder,"subject_NAME.txt")),header = F, colClasses = "numeric", col.names = "Subject")
  #Reading measurement file & selecting features
  measurement <- read.table(file.path(folder,gsub("NAME",folder,"X_NAME.txt")),header = F, colClasses = "numeric", col.names = features,check.names = FALSE)
  measurement <- measurement[,featuresIndex]
  #Reading activity file & renaming them
  activity <- read.table(file.path(folder,gsub("NAME",folder,"y_NAME.txt")),header = F, colClasses = "character", col.names = "ActivityLabel")
  activity <- renameActivity(activity)
  #Merging the three dataframes
  data<-subject
  data[names(activity)]<-activity
  data[names(measurement)]<-measurement
  data
}

# We are ready to read the data:
# We need to gather data from two folders:
folders<-c("test","train")

# We apply the function readDataFolder() to both folders
# and then we bind the data into one big dataframe
Data<-do.call(rbind,lapply(folders,readDataFolder))

# ordering data by subject and activity:
Data<-Data[order(Data$Subject,Data$Activity),]
rownames(Data) <- 1:nrow(Data)

Data$Subject<-as.factor(Data$Subject)
Data$Activity<-as.factor(Data$Activity)


## We get ready to prepare the final tidy data set:
## means as the same headers as Data: i.e., 
## [Subject Index] - [Activity's Name] - 66x[Feature's Name]

means<-data.frame(Subject=character(),Activity=character(),stringsAsFactors=FALSE)
for (i in 1:length(featuresIndex)) {
  means[,features[featuresIndex[i]]]<-numeric()
}



## We split the data according to Subject and Activity:
bySubjectAndActivity<-split(Data,paste(Data$Subject,Data$Activity))

## For each pair (subject,activity), we compute the mean of each features
## and store it in bySubjectAndActivity
index<-1
for(item in bySubjectAndActivity){
  means[index,c("Subject", "Activity")]<-c(as.character(item$Subject[1]), as.character(item$Activity[1]))
  means[index,features[featuresIndex]] <-sapply(item[,features[featuresIndex]],mean)
  index<-index+1
}
## We order means by Subject and Activity
means<-means[order(as.numeric(means$Subject),means$Activity),]

# going back and cleaning up...
setwd(file.path("..",".."))
unlink("RunAnalysis",recursive =TRUE)

# writing the result
write.table(means, file = "means.txt",row.name=FALSE)