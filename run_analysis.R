analysis <- function() {
    # Gets the dataset files
    dataUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipFile = "dataset.zip"
    if(!file.exists(zipFile)) {
        download.file(dataUrl, zipFile)
        unzip(zipFile)
    }
    
    ###############################################################
    
    ## STEP 1: Merges the training and the test sets to create
    ##         one data set.
 
    # Get data for train and test
    trainData = getData("train")
    testData = getData("test")
    
    # Bind the train and test data
    mergedData = rbind(trainData, testData)
    
    ###############################################################
    
    ## STEP 2: Extracts only the measurements on the mean and
    ##         standard deviation for each measurement.

    # Get mean/standard deviation realted feature ids and names
    extraction <- getExtraction()
    
    # Get the columnId for "activity", "volunteer" "type"
    colNames = names(mergedData)
    activityId = which(colNames == "Activity")
    volunteerId = which(colNames == "Volunteer")
    typeId = which(colNames == "type")
    
    # extract data from mergedData by column id
    mergedData = mergedData[, c(extraction$feature_id, activityId, volunteerId, typeId)]
    
    ###############################################################
    
    ## STEP3: Uses descriptive activity names to name the activities in the data set
    
    # Get the id-feature name table from activity_labels.txt
    activity<-read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors=FALSE)
    names(activity) <- c("activity", "activity_label")
    
    # change the list of "activity" from ID to descriptive name by sapply
    mergedData$Activity<-sapply(mergedData$Activity,function(id) activity[activity==id,2])
    
    ###############################################################
    
    ## STEP4: Appropriately labels the data set with descriptive variable names.
    
    # Reuse the extraction generated in step3 to rename the column name.
    # Direct query of the feature name in extaction by column_seq is ok because the column seq is not changed
    featureNames<-extraction[,2]
    # Remove "()" in the feature name
    # Replace "-" with "_"
    featureNames<-lapply(featureNames,
                         function(feature) {
                             gsub("-", "_", sub("\\(\\)", "", feature))
                         })
    for (i in 1:length(featureNames)) {
        colnames(mergedData)[i] <- featureNames[i] 
    }
    
    ###############################################################
    
    ## STEP5: Creates a second, independent tidy data set
    ##        with the average of each variable for each 
    ##        activity and each subject. 

    # Calculate the result by function aggregate()
    attach(mergedData)
    result <- aggregate(mergedData, by=list(Activity,Volunteer), FUN=mean)    
    detach(mergedData)
    
    # Trim the result
    # 1. remove last 3 columns, because the means of Activty, Volunteer, type column are meaningless
    colNames<-colnames(result)
    activityId <- which(colNames == "Activity")
    volunteerId <- which(colNames == "Volunteer")
    typeId <- which(colNames == "type")
    result <- result[,-c(activityId, volunteerId, typeId)]
    
    # 2. Set the content of 1st column as "$Activity by volunteer $Volunteer"
    result$Group.1 = paste(result$Group.1, "by volunteer", result$Group.2)
    
    # 3. Rename Column Group.1 and remove Column Group2
    result<- result[,-2]
    colnames(result)[1] <- "Activity by Volunteer"
    

    ## Output the result
    write.table(result, file = "result.txt", row.names=FALSE)
}

## This function gets the date in the %category sustdbfolder
## The data consists of: set, label, volunteer, type
## Input: "train" for getting traib data and "test" for getting test deviation
## Output: Train data for input "train"
##         Test data for input "test"
##         Null for other input
getData <- function(category) {
    # param check
    if (category != "train" & category != "test") {
        return(NULL)
    }
    
    # set the directory according to the category
    directory = paste("./UCI HAR Dataset/", category, sep="")
    
    trainX <- read.table(paste(directory, "/X_", category, ".txt", sep="")) # data set
    trainY <- read.table(paste(directory, "/y_", category, ".txt", sep="")) # activity
    volunteer <- read.table(paste(directory, "/subject_", category, ".txt", sep="")) # volunteer
    
    type = category
    typeDF <- data.frame(type)
    
    names(trainY) <- "Activity"
    names(volunteer) <- "Volunteer"
    
    dataset <- cbind(trainX, trainY, volunteer, typeDF)
}

## This function extracts the id-featureName pairs of the mean/standard deviation
## for each measurement. 
## Output: Id and name of the mean/standard deviation related features
getExtraction <- function() {
    # Read id-feature table from features.txt
    features<-read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
    # extract the id-feature pair for means
    names(features)<-c("feature_id","feature")
    meanID = grep("mean|std", features$feature)
    
    extraction <- features[meanID,]
}

