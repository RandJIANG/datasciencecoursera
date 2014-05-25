#data set 1
#read files
train <- read.table("../UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("../UCI HAR Dataset/train/y_train.txt")
test <- read.table("../UCI HAR Dataset/test/X_test.txt")
testY <- read.table("../UCI HAR Dataset/test/y_test.txt")
features <- read.table("../UCI HAR Dataset/features.txt")
activity_labels <- read.table("../UCI HAR Dataset/activity_labels.txt")

#activity description
trainYactivity <- sub("1","WALKING",trainY$V1)
trainYactivity <- sub("2","WALKING_UPSTAIRS",trainYactivity)
trainYactivity <- sub("3","WALKING_DOWNSTAIRS",trainYactivity)
trainYactivity <- sub("4","SITTING",trainYactivity)
trainYactivity <- sub("5","STANDING",trainYactivity)
trainYactivity <- sub("6","LAYING",trainYactivity)
testYactivity <- sub("1","WALKING",testY$V1)
testYactivity <- sub("2","WALKING_UPSTAIRS",testYactivity)
testYactivity <- sub("3","WALKING_DOWNSTAIRS",testYactivity)
testYactivity <- sub("4","SITTING",testYactivity)
testYactivity <- sub("5","STANDING",testYactivity)
testYactivity <- sub("6","LAYING",testYactivity)

#merge data
train$Activity <- trainYactivity
test$Activity <- testYactivity
mergeData <- merge(train, test, all = TRUE)

#shape tidy data
names(mergeData) <- features$V2
names(mergeData)[[562]] <- "Activity"

#extracts mean() and std() measurements
extractData <- mergeData[,c(1,2,3,41,42,43,81,82,83,121,122,123,161,162,163,201,214,227,240,253,266,267,268,345,346,347,424,425,426,503,516,529,542,4,5,6,44,45,46,84,85,86,124,125,126,164,165,166,202,215,228,241,254,269,270,271,348,349,350,427,428,429,504,517,530,543,562)]


#data set 2
#read and clean data
subject_train <- read.table("../UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("../UCI HAR Dataset/test/subject_test.txt")
subject <- merge(subject_train, subject_test, all = TRUE)
mergeData2 <- mergeData
mergeData2$Subject <- subject
dataset2 <- matrix(nrow=180,ncol=563)
colnames(dataset2) <- names(mergeData2)

#all means for each subject of activity "WALKING"
for(i in 1:30){
        dataSelect <- mergeData2[mergeData2$Subject==i & mergeData2$Activity == "WALKING",]
        meanRow <- colMeans(dataSelect[,1:561])
        meanRow[562] <- "LAYING"
        meanRow[563] <- i
        dataset2[i,] <- meanRow
}

#all means for each subject of activity "WALKING_UPSTAIRS"
for(i in 1:30){
        dataSelect <- mergeData2[mergeData2$Subject==i & mergeData2$Activity == "WALKING_UPSTAIRS",]
        meanRow <- colMeans(dataSelect[,1:561])
        meanRow[562] <- "WALKING_UPSTAIRS"
        meanRow[563] <- i
        dataset2[i+30,] <- meanRow
}

#all means for each subject of activity "WALKING_DOWNSTAIRS"
for(i in 1:30){
        dataSelect <- mergeData2[mergeData2$Subject==i & mergeData2$Activity == "WALKING_DOWNSTAIRS",]
        meanRow <- colMeans(dataSelect[,1:561])
        meanRow[562] <- "WALKING_DOWNSTAIRS"
        meanRow[563] <- i
        dataset2[i+60,] <- meanRow
}

#all means for each subject of activity "SITTING"
for(i in 1:30){
        dataSelect <- mergeData2[mergeData2$Subject==i & mergeData2$Activity == "SITTING",]
        meanRow <- colMeans(dataSelect[,1:561])
        meanRow[562] <- "SITTING"
        meanRow[563] <- i
        dataset2[i+90,] <- meanRow
}

#all means for each subject of activity "STANDING"
for(i in 1:30){
        dataSelect <- mergeData2[mergeData2$Subject==i & mergeData2$Activity == "STANDING",]
        meanRow <- colMeans(dataSelect[,1:561])
        meanRow[562] <- "STANDING"
        meanRow[563] <- i
        dataset2[i+120,] <- meanRow
}


#all means for each subject of activity "LAYING"
for(i in 1:30){
        dataSelect <- mergeData2[mergeData2$Subject==i & mergeData2$Activity == "LAYING",]
        meanRow <- colMeans(dataSelect[,1:561])
        meanRow[562] <- "LAYING"
        meanRow[563] <- i
        dataset2[i+150,] <- meanRow
}