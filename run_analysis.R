#Load Plyr Package for final step
library(plyr)

#Read in all of the informational data
activity_labels <- read.table("activity_labels.txt", header=FALSE)
features <- read.table("features.txt", header=FALSE)

#Read in test folder data
subject_test <- read.table("test/subject_test.txt", header=FALSE)
x_test <- read.table("test/x_test.txt", header=FALSE)
y_test <- read.table("test/y_test.txt", header=FALSE)

#Read in train folder data
x_train <- read.table("train/x_train.txt", header=FALSE)
y_train <- read.table("train/y_train.txt", header=FALSE)
subject_train <- read.table("train/subject_train.txt", header=FALSE)

#Join Xs, Ys, Subjects
allx <- rbind(x_test, x_train)
ally <- rbind(y_test, y_train)
allsubject <- rbind(subject_test, subject_train)

#Change Column Headings to something meaningful
colnames(allsubject) <- "subjectID"
colnames(activity_labels) <- c("activityID", "activityType")

#Change the ActivityID to Activity Names, and rename Column
ally[, 1] <- activity_labels[ally[ , 1], 2]
colnames(ally) <- "activity"
colnames(allx) <- features[,2]

#Create an index of Columns where the Column Name includes "mean()" or "std()"
meanandstdindex <- grep("(mean|std)\\(\\)", features[, 2])

#Create a merged data set
alldata <- cbind(allsubject, ally, allx)

#Subset by the index of mean() and std() Columns
mandsdata <- alldata[, meanandstdindex]

#Give Columns full descriptions
names(mandsdata) <- gsub("BodyBody", "Body", names(mandsdata))
names(mandsdata) <- gsub("tBodyAcc", "Time Domain Body Acceleration ", names(mandsdata))
names(mandsdata) <- gsub("tBodyGyro", "Time Domain Gyroscopic ", names(mandsdata))
names(mandsdata) <- gsub("tGravityAcc", "Time Domain Gravity Acceleration ", names(mandsdata))
names(mandsdata) <- gsub("fBodyAcc", "Frequency Domain Body Acceleration ", names(mandsdata))
names(mandsdata) <- gsub("fBodyGyro", "Frequency Domain Gyroscopic ", names(mandsdata))
names(mandsdata) <- gsub("fGravityAcc", "Frequency Domain Gravity Acceleration ", names(mandsdata))
names(mandsdata) <- gsub("-mean\\(\\)-", "Mean ", names(mandsdata))
names(mandsdata) <- gsub("-std\\(\\)-", "Standard Deviation ", names(mandsdata))
names(mandsdata) <- gsub("-correlation\\(\\)-", "Correlation ", names(mandsdata))
names(mandsdata) <- gsub("-skewness\\(\\)", " Skewness", names(mandsdata))
names(mandsdata) <- gsub("-kurtosis\\(\\)", " Kurtosis", names(mandsdata))
names(mandsdata) <- gsub("-bandsEnergy\\(\\)-", "Bands Energy ", names(mandsdata))
names(mandsdata) <- gsub("-arCoeff\\(\\)", " Autoregression Coefficient ", names(mandsdata))
names(mandsdata) <- gsub("Jerk", "Jerk ", names(mandsdata))

#Create a table of Column Means by Subject and Activity
averages <- ddply(mandsdata, .(subjectID, activity), function(x) colMeans(x[ , 3:66]))

#Write table of Column Means by Subject and Activity as a txt file as per instructions
write.table(averages, "Column Means By Activty By Subject.txt", row.name=FALSE)