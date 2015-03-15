downloadFile <- function(  ){
      url <-  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      file <- "repdata_data_Factivity.zip"
      dir <- "./Data"
      browser()
      if( !file.exists(dir)){
            dir.create(dir)
      }
      destfile <- paste(dir, file, sep="/")
      download.file(url, destfile, method="curl")
}
#browser()
activity <- vector()

# Load the CSV and clean data - make it tidy
unzip("data/activity.zip")
activity <- read.csv("Data/repdata_data_activity/activity.csv")
#activity$date <- as.Date(activity$date)

#  interval time 
activity$interval <- as.factor(activity$interval)
str(activity)

totalStepsPerDay <- tapply(activity[,1], activity[,2], sum, na.rm = TRUE)

      hist(totalStepsPerDay, xlab = "Total Steps", ylab = " Number of Days", 
           main = "Histogram of Total Steps in Day", breaks = 50 )
      
      meanSteps   = round(mean(  totalStepsPerDay))
      medianSteps = median(totalStepsPerDay)
      print(paste("Mean   steps per day ", as.character(meanSteps)))
      print(paste("Median steps per day ", as.character(medianSteps)))


newActivity <- activity


#activityLevel <- function(){

#   Compute mean activity level by time interval
#                    steps       interval
avgStepsPerInterval <- tapply(activity[,1], activity[,3], mean, na.rm = TRUE)
avgStepsPerInterval <- round(avgStepsPerInterval)

#  plot Interval vs Mean Step for weekend
plot(names(avgStepsPerInterval), avgStepsPerInterval, type = "l", xlab = "5 Minute Interval", ylab = "Mean Steps", main = "Activity Levels")
browser()
ml <- which.max(avgStepsPerInterval)
browser()
maxAvg <- activity$interval[ml]  #  determine index of interval with max average
browser()
print(paste("5 Minute Interval with maximum average number of steps is ",as.character(maxAvg)))
browser()
     
      #  determine number of missing values
      missingValues <- is.na(activity$steps)
      print(sum(missingValues))
      browser()
      intervals <- unique(activity$interval)
      avgStepsPerInterval <- round(avgStepsPerInterval)
      names(avgStepsPerInterval) <- NULL
      avgInt <- data.frame(intervals, avgStepsPerInterval)
      #impute missing values
      #newActivity <- activity
browser()
      for(indx in which(missingValues)){
            #browser()
            if(missingValues[indx]){
                  invVal <- newActivity[indx,3]  #  identify the inverval value
                  avgStep <- avgInt[avgInt$intervals == invVal,2]
                  newActivity[indx,1] <- avgStep
                  #browser()
            }
            
      }
browser()
      days <- unique(newActivity$date)
ntotalStepsPerDay <- tapply(newActivity[,1], newActivity[,2], sum)

      
     
      hist(ntotalStepsPerDay, xlab = "Total Steps", ylab = " Number of Days", 
           main = "Histogram of Total Steps in Day with NA Removed", breaks = 50 )
      
      #browser()
      meanSteps   = mean(  ntotalStepsPerDay)
      medianSteps = median(ntotalStepsPerDay)
      print(paste("Mean   steps per day ", as.character(meanSteps)))
      print(paste("Median steps per day ", as.character(medianSteps)))
      #browser()

wkdiff <- ntotalStepsPerDay - totalStepsPerDay
nomen <- names(totalStepsPerDay)
browser()
plot(nomen, wkdiff, type = "l", xlab = "Day", ylab = "Difference in Total Steps", main = "Effect of Removing NA on Steps per Day" )

      
      
#}
#browser()
weekdy <- weekdays( as.Date(newActivity$date))
weekends <- weekdy == "Sunday" | weekdy == "Saturday"
#browser()
newActivity$wkend <- as.factor(c("weekday","weekend"))
newActivity[weekends,]$wkend <- as.factor("weekend")
newActivity[!weekends,]$wkend <- as.factor("weekday")

library(dplyr)
wkpart <- filter(newActivity, wkend == "weekend")
rm(list)
#                    steps       interval
wkendMeans <- tapply(wkpart[,1], wkpart[,3], mean)
plot(names(wkendMeans), wkendMeans, type = "l", xlab = "Interval", ylab = "Mean Steps", main = "Weekend Activity Levels")

wkpart <- filter(newActivity, wkend == "weekday")
#                    steps       interval
wkdayMeans <- tapply(wkpart[,1], wkpart[,3], mean)
plot(names(wkdayMeans), wkdayMeans, type = "l", xlab = "Interval", ylab = "Mean Steps", main = "Weekday Activity Levels")

wkdiff <- wkendMeans - wkdayMeans
plot(names(wkdiff), wkdiff, type = "l", xlab = "Interval", ylab = "Difference in Mean Steps", main = "Activity Level Differences (Weekend - Weekday)" )


ntotalStepsPerDay <- tapply(newActivity[,1], newActivity[,2], sum, na.rm = TRUE)
browser()

hist(ntotalStepsPerDay, xlab = "Total Steps", ylab = " Number of Days", 
     main = "Histogram of Total Steps in Day with NAs Removed", breaks = 50 )


newMeanSteps   = round(mean(  ntotalStepsPerDay))
newMedianSteps = median(ntotalStepsPerDay)
print(paste("Mean   steps per day with NAs removed ", as.character(newMeanSteps)))
print(paste("Median steps per day with NAs removed ", as.character(newMedianSteps)))


browser()



