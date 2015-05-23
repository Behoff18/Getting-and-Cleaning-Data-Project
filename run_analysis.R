##  run_analysis.R 

##  1. Merges the training and the test sets to create one data set.
##  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##  3. Uses descriptive activity names to name the activities in the data set
##  4. Appropriately labels the data set with descriptive variable names. 
##  5. From the data set in step 4, creates a second, independent tidy data set with the average of 
##     each variable for each activity and each subject.

library(dplyr)

mainFolder <- "UCI HAR Dataset\\"
dataFiles <- paste(mainFolder, c("train\\x_train.txt",  "test\\x_test.txt"),  sep="")
labelFiles <- paste(mainFolder, c("train\\y_train.txt", "test\\y_test.txt"), sep="")

# Just in case it is sitting around in memory
if (exists("allData")) rm("allData")

## Fetch the list of variable names for the test and train files
fields <- read.table(paste(mainFolder, "features.txt", sep=""))
## Get rid of () and replace - with _
fields$V2 <- gsub("-", "_", fields$V2, fixed=TRUE)
fields$V2 <- gsub("(", "", fields$V2, fixed=TRUE)
fields$V2 <- gsub(")", "", fields$V2, fixed=TRUE)

## Merge the training and test sets
for (i in 1:2) {
    temp <- read.table(dataFiles[i])
    
    names(temp) <- fields$V2

    temp2 <- read.table(labelFiles[i])
    temp2 <- cbind(temp2, temp)
    
    if (exists("dataTable")) {
       allData <- rbind(allData, temp2)
    }
    else  {
       allData <- temp2
    }
}
# clean up memory
rm("temp", "temp2", i)

## Add the activity names
activities <- read.table(paste(mainFolder, "activity_labels.txt", sep=""))

allData <- merge(activities, allData)
allData <- rename(allData, activityCode=V1, activity=V2)

## Get the list we want 
fieldsWanted <- c(1,2)
fieldsWanted <- union(fieldsWanted, grep("_(mean[^F])|(std)", fields$V2)+2)

## Trim down the columns to only those wanted
tidyData <- allData[, fieldsWanted]


## Create a new data set with the average of each activity
groups <- group_by(tidyData, activityCode, activity)
tidyData <- summarise_each(groups, funs(mean))


## Save it to a file for upload
write.table(tidyData, file="tidyData.txt", row.name=FALSE)

#Also crteate a long version
tidyDataLong <- melt(tidyData, id.vars=c("activityCode", "activity"), measure.vars=c(3:ncol(tidyData)))
write.table(tidyDataLong, file="tidyDataLong.txt", row.name=FALSE)
