library(caret)
library(rattle)

WindowsMultiProcess <- function() {
      print("Setting Up Multiprocess for Windows")
      library(doSNOW) 
      library(foreach) 
      c1 <- makeCluster(detectCores()) 
      registerDoSNOW(c1)        
}

UnixMultiProcess <- function(name) {
      print(paste0("Setting up multicore processing for Unix style OS: ", name))
      library(doParallel)
      c1 <- makeCluster(detectCores())
      registerDoParallel(c1)
}

switch(Sys.info()[['sysname']], 
      Windows = { WindowsMultiProcess()},
      Linux = { UnixMultiProcess("Linux")},
      Darwin = { UnixMultiProcess("Mac OS X")})
set.seed(32323)
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="train_data.csv", method = "curl")
training <- read.csv("train_data.csv")

#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="test_data.csv", method = "curl")
testing <- read.csv("test_data.csv")
# Exploratory


# Names of columns not complete

names(training[,complete.cases(training)])
length(names(training[!complete.cases(training),]))

# Find factor variables 
temp <- sapply(training, class)
cols <- grep("factor", temp)
names(training[,cols])



clean.data <- train[lapply(train, function(x) sum(is.na(x)) / length(x)) == 0]
clean.data$user_name <- NULL
clean.data$new_window <- NULL
clean.data$num_window <- NULL
remove <- grepl("^X|timestamp|window", names(clean.train))
clean.data <- clean.data[, !remove]
clean.data <- clean.data[, sapply(clean.data, is.numeric)]
clean.data$classe <- train$classe



clean.data <- training[lapply(training, function(x) sum(is.na(x)) / length(x)) == 0]
clean.data$user_name <- NULL
clean.data$new_window <- NULL
clean.data$num_window <- NULL
remove <- grepl("^X|timestamp|window", names(clean.data))
clean.data <- clean.data[, !remove]
clean.data <- clean.data[, sapply(clean.data, is.numeric)]
clean.data$classe <- training$classe
# Break training into a test and train
inTrain <- createDataPartition(y = clean.data$classe, p=.75, list=FALSE)
clean.train <- clean.data[inTrain,]
clean.test <- clean.data[-inTrain,]
dim(train)
dim(test)



clean.test <- testing[lapply(testing, function(x) sum(is.na(x)) / length(x)) == 0]

clean.test$user_name <- NULL
clean.test$new_window <- NULL
clean.test$num_window <- NULL
clean.test$X <- NULL
remove <- grepl("^X|timestamp|window", names(clean.test))
clean.test <- clean.test[, !remove]
clean.test <- clean.test[, sapply(clean.test, is.numeric)]
clean.test$classe <- testing$problem_id

clean.test <- testing

#trainRaw <- training[, colSums(is.na(training)) ==0 ]
#temp <- testing[, names(testing) %in% names(clean.train)]
#temp$problem_id <- NULL
#temp$problem_id <- testing$problem_id


# Training
#folds <- createTimeSlices(y = training$classe, initialWindow = 20, horizon = 10)
#fd.train <- folds[[1]]

## K-folds ???

# Repeated cross validaion
ctrlRepeat <- trainControl(method = "repeatedcv", repeats=2)

# Leave one out Cross Validation
ctrl <- trainControl(method="LOOCV")
# Cross validation
ctrl <- trainControl(method="cv", repeats = 3)
#system.time(modelFit <- train(classe ~ ., method = "rpart", data=training, trControl = ctrl, preProc = c("center", "scale")))


modelRF <- train(classe ~ ., data = clean.train, method = "rf", trControl = ctrl)
set.seed(32323)
modelGBM <- train(classe ~ ., data = clean.train, method = "gbm", trControl = ctrl, preProc = pre, verbose=FALSE)
set.seed(32323)
modelSVM <- train(classe ~ ., data = clean.train, method = "svmLinear", trControl = ctrl, preProc = pre, maximize = TRUE)
set.seed(32323)
modelLVQ <- train(classe ~ ., data = clean.train, method="lvq", trControl=ctrlRepeat, preProc = pre)
plot(modelRF)

results <- resamples(list(RF = modelRF, GBM = modelGBM, SVM = modelSVM))
summary(results)
predictions <- predict(modelRf$finalModel, newdata=clean.test)
table(predictions, clean.test)

confusionMatrix(predictions, test$classe)

randomForest(classe ~ ., data=clean.train, importance=T )


ozone <- clean.train[order(clean.train$classe),]

