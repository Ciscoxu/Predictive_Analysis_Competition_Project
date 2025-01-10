setwd("C:/Users/tuqqa/Desktop/Rdata")
library(caret)
library(dplyr)
library(randomForest)

data = read.csv('analysisData.csv')
scoringData = read.csv('scoringdata.csv')  
##observe data
lis <- as.data.frame(sapply(data,class))


##data cleaning
##turn power and torque column to numeric for both data
data$power <- as.numeric(gsub(" hp.*", "", data$power))
data$torque <- as.numeric(gsub(" lb-ft.*", "", data$torque))

scoringData$power <- as.numeric(gsub(" hp.*", "", scoringData$power))
scoringData$torque <- as.numeric(gsub(" lb-ft.*", "", scoringData$torque))

## find numeric and integer column name, give name as numeric_columns
numeric_columns <- which(sapply(data, function(x) is.numeric(x) || is.integer(x)))

## get new data frame of analysis data and scoring data with only numeric and integer column
numedata <- data[,numeric_columns]
numescore <- scoringData[,numeric_columns]

##fill na value for each of numeric and integer column by using bagimpute.
newnum <- predict(preProcess(numedata,method = 'bagImpute'),newdata = numedata)
newnumscore <-  predict(preProcess(numescore,method = 'bagImpute'),newdata = numescore)


##check number of null value character column, get rid of those bigger than 5000
na_count <-sapply(data, function(y) (sum(length(which(y=="")))<5000||sum(length(which(is.na(y))))))

##deal with non-numerical or non-integer analysis data
newnum$isCab <- as.factor(data$isCab)
newnum$is_cpo <- as.factor(data$is_cpo)
newnum$is_new <- as.factor(data$is_new)
newnum$has_accidents <- as.factor(data$has_accidents)
newnum$engine_type <- data$engine_type
newnum$transmission_display <- data$transmission_display

newnum$isCab <- ifelse(newnum$isCab=="", "No Data", newnum$isCab)
newnum$is_cpo <- ifelse(newnum$is_cpo=="", "No Data", newnum$is_cpo)
newnum$is_new <- ifelse(newnum$is_cpo=="", "No Data", newnum$is_new)
newnum$has_accidents <- ifelse(newnum$has_accidents=="", "No Data", newnum$has_accidents)
newnum$engine_type <- ifelse(newnum$engine_type=="", "No Data", newnum$engine_type)
newnum$transmission_display <- ifelse(newnum$transmission_display=="", "No Data", newnum$transmission_display)

  
## deal with non-numerical or non-integer scoring data
newnumscore$is_new <- as.factor(scoringData$is_new)
newnumscore$is_cpo <- as.factor(scoringData$is_cpo)
newnumscore$isCab <- as.factor(scoringData$isCab)
newnumscore$has_accidents <- as.factor(scoringData$has_accidents)
newnumscore$engine_type <- scoringData$engine_type
newnumscore$transmission_display <- scoringData$transmission_display

newnumscore$is_new <- ifelse(newnumscore$is_cpo=="", "No Data", newnumscore$is_new)  
newnumscore$is_cpo <- ifelse(newnumscore$is_cpo=="", "No Data", newnumscore$is_cpo)
newnumscore$isCab <- ifelse(newnumscore$isCab=="", "No Data", newnumscore$isCab)
newnumscore$has_accidents <- ifelse(newnumscore$has_accidents=="", "No Data", newnumscore$has_accidents)  
newnumscore$engine_type <- ifelse(newnumscore$engine_type=="", "No Data", newnumscore$engine_type)
newnumscore$transmission_display <- ifelse(newnumscore$transmission_display=="", "No Data", newnumscore$transmission_display)


##data analyse
##use tuned process to find best mtry
trControl = trainControl(method = 'cv', number = 5)
tuneGrid = expand.grid(mtry = 1:ncol(newnum)-1)
tm = train(price~.,data=newnum,method="ranger",num.trees=200,trControl=trControl,tuneGrid=tuneGrid)
mtry=tm$bestTune$mtry

##use random forest to predict the model
model <- randomForest(price~.,newnum,mtry=mtry,ntree=200)

##predict by data scoring
pred = predict(model,data=newnumscore)


##submission process
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)