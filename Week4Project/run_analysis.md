---
title: "Getting and Cleaning Data Course Project"


output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---
Run Analysis : HTML File

By: Raj Kumar Pandit


##Preliminaries


####Packages Initialization


```r
packages <- c("data.table", "reshape2")
sapply (packages, require, character.only=TRUE, quietly=TRUE)
```

```
## data.table   reshape2 
##       TRUE       TRUE
```

```r
library(knitr)
```

####Set Path.


```r
pth <- getwd()
pth
```

```
## [1] "C:/ImportantFiles/RK/Education/DataScience/Course-3_dataClean/week4/Project"
```


##Get the data


Downloading the file. 


```r
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "Dataset.zip" )
```

Unzipping the file- The R way :) 


```r
executable <- file.path("C:", "Program Files", "7-Zip", "7zFM.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path,  "Dataset.zip"), "\""))
system(cmd)
```

TThe file are copied in folder `UCI HAR Dataset` under the same working directpory. Lets list all the files here inside.


```r
pth <- file.path(pth, "UCI HAR Dataset")
list.files(pth, recursive=TRUE)
```

```
##  [1] "activity_labels.txt"                         
##  [2] "features.txt"                                
##  [3] "features_info.txt"                           
##  [4] "README.txt"                                  
##  [5] "test/Inertial Signals/body_acc_x_test.txt"   
##  [6] "test/Inertial Signals/body_acc_y_test.txt"   
##  [7] "test/Inertial Signals/body_acc_z_test.txt"   
##  [8] "test/Inertial Signals/body_gyro_x_test.txt"  
##  [9] "test/Inertial Signals/body_gyro_y_test.txt"  
## [10] "test/Inertial Signals/body_gyro_z_test.txt"  
## [11] "test/Inertial Signals/total_acc_x_test.txt"  
## [12] "test/Inertial Signals/total_acc_y_test.txt"  
## [13] "test/Inertial Signals/total_acc_z_test.txt"  
## [14] "test/subject_test.txt"                       
## [15] "test/X_test.txt"                             
## [16] "test/y_test.txt"                             
## [17] "train/Inertial Signals/body_acc_x_train.txt" 
## [18] "train/Inertial Signals/body_acc_y_train.txt" 
## [19] "train/Inertial Signals/body_acc_z_train.txt" 
## [20] "train/Inertial Signals/body_gyro_x_train.txt"
## [21] "train/Inertial Signals/body_gyro_y_train.txt"
## [22] "train/Inertial Signals/body_gyro_z_train.txt"
## [23] "train/Inertial Signals/total_acc_x_train.txt"
## [24] "train/Inertial Signals/total_acc_y_train.txt"
## [25] "train/Inertial Signals/total_acc_z_train.txt"
## [26] "train/subject_train.txt"                     
## [27] "train/X_train.txt"                           
## [28] "train/y_train.txt"
```


##Reading the files


####Read the subject files.


```r
dataSubjectTrain    <- fread(file.path(pth, "train", "subject_train.txt"))
dataSubjectTest  <- fread(file.path(pth, "test" , "subject_test.txt" ))
```

####Read the activity files.


```r
dataActivityTrain <- fread(file.path(pth, "train", "Y_train.txt"))
dataActivityTest  <- fread(file.path(pth, "test" , "Y_test.txt" ))
```

####Read the data files. 


```r
fileToDataTable <- function (fileName) {
	df <- read.table(fileName)
	dt <- data.table(df)
}
dataTrain <- fileToDataTable(file.path(pth, "train", "X_train.txt"))
dataTest  <- fileToDataTable(file.path(pth, "test" , "X_test.txt" ))
```


##Read Data


####Concatenating the data tables.


```r
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(dataSubject, "V1", "subject")
dataActivity <- rbind(dataActivityTrain, dataActivityTest)
setnames(dataActivity, "V1", "activityNum")
dt <- rbind(dataTrain, dataTest)
```

####Merging columns and setting key.


```r
dataSubject <- cbind(dataSubject, dataActivity)
dt <- cbind(dataSubject, dt)
setkey(dt, subject, activityNum)
```


##Mean and standard deviation


####For the mean and standard deviation, read the `features.txt` file


```r
dataFeatures <- fread(file.path(pth, "features.txt"))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))

dataFeatures <- dataFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
```

####Creating a columns in DataFeature to be able to link to `dt`.


```r
dataFeatures$featureCode <- dataFeatures[, paste0("V", featureNum)]
```

####Subset these variables using variable names.


```r
select <- c(key(dt), dataFeatures$featureCode)
dt <- dt[, select, with=FALSE]
```


####Use descriptive activity names


Read `activity_labels.txt` file to set descriptive names to the activities.


```r
dataActivityNames <- fread(file.path(pth, "activity_labels.txt"))
setnames(dataActivityNames, names(dataActivityNames), c("activityNum", "activityName"))
```


Merging activity labels.


```r
dt <- merge(dt, dataActivityNames, by="activityNum", all.x=TRUE)
```

Add `activityName` as a key.


```r
setkey(dt, subject, activityNum, activityName)
```

Melt the data table to reshape it and adding new columns


```r
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dataFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
```

Seperate features from `featureName` using the helper function `grepthis`.


```r
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories

y <- matrix(seq(1, 2), nrow=2)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories

y <- matrix(seq(1, 3), nrow=3)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
```

Check to make sure all possible combinations of `feature` are accounted for by all possible combinations of the factor class variables.


```r
r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2
```

```
## [1] TRUE
```




###Create a tidy data set


Data set with the average of each variable for each activity-subject.


```r
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
```

###Code Book


```r
save.image("myWorkSpace.RData")
knit("makeCodebook.Rmd", output = "codebook.md", encoding = "ISO8859-1")
```

```
## 
## 
## processing file: makeCodebook.Rmd
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |.....                                                            |   8%
##   ordinary text without R code
## 
##   |                                                                         |...........                                                      |  17%
## label: unnamed-chunk-20
##   |                                                                         |................                                                 |  25%
##   ordinary text without R code
## 
##   |                                                                         |......................                                           |  33%
## label: unnamed-chunk-21
##   |                                                                         |...........................                                      |  42%
##   ordinary text without R code
## 
##   |                                                                         |................................                                 |  50%
## label: unnamed-chunk-22
##   |                                                                         |......................................                           |  58%
##   ordinary text without R code
## 
##   |                                                                         |...........................................                      |  67%
## label: unnamed-chunk-23
##   |                                                                         |.................................................                |  75%
##   ordinary text without R code
## 
##   |                                                                         |......................................................           |  83%
## label: unnamed-chunk-24
##   |                                                                         |............................................................     |  92%
##   ordinary text without R code
## 
##   |                                                                         |.................................................................| 100%
## label: save
```

```
## output file: codebook.md
```

```
## [1] "codebook.md"
```

```r
markdownToHTML("codebook.md", "codebook.html")
```
