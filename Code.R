library(caret)
library(randomForest)
set.seed(1234)

input<-read.csv("~/pml-training.csv",na.strings=c("NA","","#DIV/0!"))
validation<-read.csv("~/pml-testing.csv",na.strings=c("NA","","#DIV/0!"))

summary(input)
str(input)

input<-input[,8:length(input)]
validation<-validation[,8:length(validation)]
excl<-which(colSums(is.na(input))>10)
input<-input[,-excl]
validation<-validation[,-excl]

trainset<-createDataPartition(input$classe,p=0.9,list=F)
training<-input[trainset,]
testing<-input[-trainset,]

ctrler<-trainControl(method="cv",allowParallel=TRUE, number = 10,repeats=10)
modrf<-train(classe~.,method="rf",data=training,trControl=ctrler)

predictionTrain <- predict(modrf, training)
confusionMatrix(predictionTrain, training$classe)$table
confusionMatrix(predictionTrain, training$classe)$overall

predictionTest <- predict(modrf, testing)
confusionMatrix(predictionTest, testing$classe)$table
confusionMatrix(predictionTest, testing$classe)$overall

predictionResults <- predict(modrf, validation)
predictionResults
