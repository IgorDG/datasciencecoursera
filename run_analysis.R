run_analysis <- function()
{
  ## Read the files
  path <- paste(getwd(),"/UCI HAR Dataset/", sep = "")
  features <- read.table(paste(path,"features.txt",sep=""))
  activityLabels <- read.table(paste(path,
                                     "activity_labels.txt",
                                     sep=""), 
                               col.names=c("activity", "activityName"))
  
  trainX <- read.table(paste(path,"train/X_train.txt", sep = ""),
                       col.names = features[,2])
  trainY <- read.table(paste(path,"train/y_train.txt", sep = ""))
  trainSubject <- read.table(paste(path,"train/subject_train.txt", 
                                   sep = ""))
  
  testX <- read.table(paste(path,"test/X_test.txt", sep = ""),
                      col.names = features[,2])
  testY <- read.table(paste(path,"test/y_test.txt", sep = ""))
  testSubject <- read.table(paste(path,"test/subject_test.txt", 
                                  sep = ""))
  
  
  
  ## Keeping just the mean and standar deviation columns
  trainXMean <- trainX[,grepl(features[,2],pattern="mean")]
  trainXStd <- trainX[,grepl(features[,2],pattern="std")]
  trainX <- cbind(trainXMean,trainXStd)
  
  testXMean <- testX[,grepl(features[,2],pattern="mean")]
  testXStd <- testX[,grepl(features[,2],pattern="std")]
  testX <- cbind(testXMean,testXStd)
  
  
  ## Assigning the subject
  trainX[,"subject"] <- trainSubject
  trainX[,"activity"] <- trainY
  
  testX[,"subject"] <- testSubject
  testX[,"activity"] <- testY

  ## Merging test and train
  total <- rbind(testX,trainX)
  
  ## Creating a new tidy data set aggregating by subject and activity
  totalAgg <- aggregate(total, 
                        by=list(subjectG=total$subject,
                                activityG = total$activity), 
                        FUN=mean)
    
  ## Assigning the activity label
  total <- merge(total,activityLabels, by="activity")
  totalAgg <- merge(totalAgg,activityLabels, by="activity")
  
  ## Ordering and cleaning the columns a little
  colIndex <- grep("activityName", names(totalAgg))
  totalAgg <- totalAgg[, c(colIndex, (1:ncol(totalAgg))[-colIndex])]
  totalAgg$activityG <- NULL
  totalAgg$activity <- NULL
  totalAgg$subject <- NULL
  
  ## Writing the tidy results in a files called output.txt 
  ## and outputAgg.txt
  write.table(total, 
              file = paste(getwd(),"/output.txt", sep = ""), 
              sep ="\t")
  write.table(totalAgg, 
              file = paste(getwd(),"/outputAgg.txt", sep = ""), 
              sep ="\t")

}