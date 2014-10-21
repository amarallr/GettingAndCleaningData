# setwd("C:\\courseraworkspace\\getdata")
# source("run_analysis.R")

library(sqldf)
##=============================================================================
## Verifies if the data directory already exists, download data file and 
## unzip it.
##=============================================================================
getDataset <- function()
{
        if (!file.exists("Data")) {
        
                fileDir <- "Data"
                
                fileName <- "Data\\ProjectDataset.zip"
        
                url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                
                
                ## Creates the data file directory
                dir.create(fileDir)
                
                ## Downloads the project data file
                download.file(url, fileName)
                
                ## Unzip the the data project file
                unzip(zipfile=fileName, exdir=fileDir)
                
                        
        }
        
        return(TRUE)
}

##=============================================================================
## Reads the data files project.
##=============================================================================
readDataset <- function() {
        
        ## Activity labels
        dataPathActivityLabels  <- "Data\\UCI HAR Dataset\\activity_labels.txt"

        ## Features
        dataPathFeatures        <- "Data\\UCI HAR Dataset\\features.txt"
        
        ## Trainning data.
        dataPathTrainSubject    <- "Data\\UCI HAR Dataset\\train\\subject_train.txt"
        dataPathTrainX          <- "Data\\UCI HAR Dataset\\train\\X_train.txt"
        dataPathTrainY          <- "Data\\UCI HAR Dataset\\train\\Y_train.txt"
        
        ## Test data.
        dataPathTestSubject     <- "Data\\UCI HAR Dataset\\test\\subject_test.txt"
        dataPathTestX           <- "Data\\UCI HAR Dataset\\test\\X_test.txt"
        dataPathTestY           <- "Data\\UCI HAR Dataset\\test\\Y_test.txt"
        
        
        ## Activity labels
        if(!exists("dsActivityLabel")) {
                print(paste0("Reading ", dataPathActivityLabels, " ..."))
                dsActivityLabel         <<- read.table(dataPathActivityLabels)
                names(dsActivityLabel)  <<- c("id", "activityName") 
        }
        
        ## Features
        if(!exists("dsFeatures")) {
                print(paste0("Reading ", dataPathFeatures, " ..."))
                dsFeatures  <<- read.table(dataPathFeatures)
                names(dsFeatures) <<- c("id", "featureName")
        }
        
        
        ## Trainnig data
        if(!exists("dsTrainSubject")) {
                print(paste0("Reading ", dataPathTrainSubject, " ..."))
                dsTrainSubject          <<- read.table(dataPathTrainSubject)
                names(dsTrainSubject)   <<- c("idSubject")
        }

        if(!exists("dsTrainX")) {
                print(paste0("Reading ", dataPathTrainX, " ..."))
                dsTrainX        <<- read.table(dataPathTrainX)
                names(dsTrainX) <<- dsFeatures$featureName                
        }
                
        if(!exists("dsTrainY")) {
                print(paste0("Reading ", dataPathTrainY, " ..."))
                dsTrainY        <<- read.table(dataPathTrainY)
                names(dsTrainY) <<- c("idActivity")
        }

        ## Test data
        if(!exists("dsTestSubject")) {
                print(paste0("Reading ", dataPathTestSubject, " ..."))
                dsTestSubject           <<- read.table(dataPathTestSubject)
                names(dsTestSubject)    <<- c("idSubject")
                
        }
        
        if(!exists("dsTestX")) {
                print(paste0("Reading ", dataPathTestX, " ..."))
                dsTestX        <<- read.table(dataPathTestX)
                names(dsTestX) <<- dsFeatures$featureName
        }
        
        if(!exists("dsTestY")) {
                print(paste0("Reading ", dataPathTestY, " ..."))
                dsTestY        <<- read.table(dataPathTestY)
                names(dsTestY) <<- c("idActivity")
                
                
        }
                
        
}

##=============================================================================
## 1. Merges the training and the test sets to create one data set.
## indTestTrain: 0 - Train / 1 - Test
##=============================================================================
mergeDataset <- function() {
        
        print("Merging X datasets...")
        dsTrainX$indTestTrainX   <<- 0 # 0 - Train / 1 - Test
        dsTestX$indTestTrainX    <<- 1 # 0 - Train / 1 - Test
        dsX         <<- rbind(dsTrainX, dsTestX)
        
        
        print("Merging Y datasets...")
        dsTrainY$indTestTrainY   <<- 0 # 0 - Train / 1 - Test
        dsTestY$indTestTrainY    <<- 1 # 0 - Train / 1 - Test
        dsY         <<- rbind(dsTrainY, dsTestY)
        
        
        print("Merging subject datasets...")
        dsTrainSubject$indTestTrainS   <<- 0 # 0 - Train / 1 - Test
        dsTestSubject$indTestTrainS    <<- 1 # 0 - Train / 1 - Test
        dsSubject   <<- rbind(dsTrainSubject, dsTestSubject)
        
}


##=============================================================================
## 2. Extracts only the measurements on the mean and standard deviation for 
## each measurement. 
##=============================================================================
extractMeasures <- function() {
        dsMeasures <<- sqldf("   select  id 
                                from    dsFeatures 
                                where   upper(featureName) like '%MEAN%'
                                or      upper(featureName) like '%STD%'")
        
        dsXAux <- dsX[,dsMeasures$id]
        
        dsX    <<- cbind(dsXAux, dsX$indTestTrainX)
}


##=============================================================================
## 3. Uses descriptive activity names to name the activities in the data set
##=============================================================================
setActivityName <- function() {
        
        dsAux <- sqldf(" select  b.idActivity, a.activityName, b.indTestTrainY 
                from    dsActivityLabel a, dsY b
                where   a.id = b.idActivity");
        
        dsY <<- dsAux        
        
}


##=============================================================================
## 4. Appropriately labels the data set with descriptive variable names. 
## -->> Already done in funtion "readDataset()". 
##=============================================================================

##=============================================================================
## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject
##=============================================================================
createTidyDataset <- function() {
        dsTidyDataset           <<- cbind(dsX, dsY, dsSubject)
        
        dsTidyDatasetAvg        <<- aggregate(dsTidyDataset[,1:ncol(dsX)-1], list(activityName=dsTidyDataset$activityName, idSubject=dsTidyDataset$idSubject), mean)
        
        write.csv(dsTidyDatasetAvg, file = ".\\tidyDataset.csv", row.names=FALSE)
        
}


##=============================================================================
## Main code that calls another functions.
##=============================================================================
main <- function() {
        
        ## Verifies if the data directory already exists, download data file and 
        ## unzip it.
        print("Getting the data files...")
        getDataset()
        
        ## Reads the data files project.
        print("Reading the data files...")
        readDataset()
        
        ## 1. Merges the training and the test sets to create one data set.
        print("1. Merging the datasets...")
        mergeDataset()
        
        ## 2. Extracts only the measurements on the mean and standard deviation 
        ## for each measurement. 
        print("2. Extracting measures...")
        extractMeasures()
        
        ## 3. Uses descriptive activity names to name the activities in the 
        ## data set
        print("3. Setting activity names...")
        setActivityName()

        ## 4. Appropriately labels the data set with descriptive variable 
        ## names. 
        ## -->> Already done in funtion "readDataset()". 
        
        ## 5. From the data set in step 4, creates a second, independent tidy 
        ## data set with the average of each variable for each activity and 
        ## each subject.
        createTidyDataset()
}



