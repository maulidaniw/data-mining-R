data <- read.csv('PimaIndiansDiabetes.csv')
summary(data)

#Duplicate
library(dplyr)
#menghapus data duplikat untuk data sama pada semua kolom
data_clear <- data %>% distinct()
#menghapus data duplikat untuk data sama pada kolom tertentu
data_clear2 <- data %>% distinct(glucose,pressure,triceps,insulin,.keep_all = TRUE)
data_clear3 <- data %>% distinct(pressure,.keep_all = TRUE)
sapply(data, function(x) length(unique(x)))

#Missing Value
sapply(data_clear, function(x) sum(is.na(x)))
#Fill missing value with mean
for (var in 1:ncol(data_clear)) {
  data_clear[is.na(data_clear[,var]),var] <- mean(data_clear[,var], na.rm = TRUE)
}
sapply(data_clear, function(x) sum(is.na(x)))

#Drop missing value
data_clear_drop <- na.omit(data)

#Drops Features
dropf <- c("triceps", "mass", "pregnants","Diabetes")
data_dropf <-data_clear[,!(names(data_clear) %in% dropf)]

#Dimention Reduction
library(caret)
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(data_dropf, method=c("pca"))
# summarize transform parameters
# print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, data_dropf)
# summarize the transformed dataset
summary(transformed)

#Categorical Features
#Label Encoder
data_cat <- data_clear
level <- c("Group 0","Group 1","Group 2","Group 3","Group 4")
data_cat$pregnants <- factor(data_cat$pregnants, levels = level)
data_cat$pregnants <- as.numeric(data_cat$pregnants)

#One Hot Encoder
library(tidyr)
library(dplyr)
data_cat2 <- data_clear %>% mutate(value = 1)  %>% spread(pregnants, value,  fill = 0 ) 

library(RWeka)

#split data
set.seed(1234)
#Contoh
smp_size <- floor(0.7 * nrow(data_cat))
train_ind <- sample(seq_len(nrow(data_cat)), size = smp_size)
train <-data_cat[train_ind, ]
test <- data_cat[-train_ind, ]

#Decision Tree
#CART

library(rpart)
library(rpart.plot)
train2 <- train[1:100,5:9]
test2 <- test[1:100,5:9]
fit <- rpart(Diabetes~., data = train2, parms = list( split = 'gini'))
rpart.plot(fit)
summary(fit)
test_pred <- predict(fit, type='class', data = test2[,1:4] )
table(test_pred, test2$Diabetes)
confusionMatrix(test_pred, test2$Diabetes)

#C45
library(RWeka)
c45model <- J48(Diabetes~., data = train2)
print(c45model)
#plot model
library(partykit)
plot(c45model)
#make prediction
c45predict <- predict(c45model, test2[,1:4],type="class")
#accuration
table(c45predict, test2$Diabetes)
confusionMatrix(c45predict, test2$Diabetes)


#using caret train()
library(caret)
trainControl <- trainControl(method="cv", number=4)
cFit <- train(Diabetes ~., method="rpart", data=train2,  
              trControl = trainControl, parms = list( split = 'gini'))
cpred <- predict(cFit, data = test2[,1:4])
table(cpred, test2$Diabetes)
confusionMatrix(cpred, test2$Diabetes)

#KNN
library(caret)
train2 <- train
test2 <- test

#fit model
knnmodel <- knn3(Diabetes~., data=train2, k=5)
print(knnmodel)
#make prediction
knnpredict <- predict(knnmodel, test2[,1:8], type="class")
#accuracy
table(knnpredict, test2$Diabetes)
confusionMatrix(knnpredict, test2$Diabetes)

#using train()
library(caret)
trainControl <- trainControl(method="cv", number=5)
fit.knn <- train(Diabetes~., data=train2, method="knn", 
                 metric="Accuracy", preProcess=c("center", "scale"), 
                 trControl=trainControl)
# summarize fit
print(fit.knn)
fit.knn$finalModel
#make prediction
knnpredict2 <- predict(fit.knn, newdata=test2[,1:8])
#accuracy
table(knnpredict2, test2$Diabetes)
confusionMatrix(knnpredict2, test2$Diabetes)

#Naive Bayes
library(e1071)
train2 <- train
test2 <- test

#fit model
nbmodel <- naiveBayes(Diabetes~., data=train2)
print(nbmodel)
#make prediction
nbpredict <- predict(nbmodel, test2[,1:8], type="class")
#accuracy
table(nbpredict, test2$Diabetes)
confusionMatrix(nbpredict, test2$Diabetes)

#using caret train()
library(caret)
library(klaR)
trainControl <- trainControl(method="cv", number=5)
fit.nb <- train(Diabetes~., data=train2, method="nb", 
                metric="Accuracy", trControl=trainControl)
# summarize fit
print(fit.nb)
fit.nb$finalModel
# make predictions
pred.nb <- predict(fit.nb, test2[,1:8])
# summarize accuracy
table(pred.nb, test2$Diabetes)
confusionMatrix(pred.nb, test2$Diabetes)

#RIPPER
library(RWeka)
rb_JRip <- JRip(Diabetes ~ ., data = train2)
rb_JRip
summary(rb_JRip)
rb_jrippred <- predict(rb_JRip, test2[,1:8])
table(rb_jrippred, test2$Diabetes)
confusionMatrix(rb_jrippred, test2$Diabetes)

#1R
library(RWeka)
rb_oneR <- OneR(Diabetes ~ ., data = train2)
rb_oneR
summary(rb_oneR)
rb_oneRpred <- predict(rb_oneR, test2[,1:8])
table(rb_oneRpred, test2$Diabetes)
confusionMatrix(rb_oneRpred, test2$Diabetes)

#C50
library(C50)
rb_c50rules <- C5.0(Diabetes ~ ., data = train2, rules = TRUE)
rb_c50rules
summary(rb_c50rules)
rb_c50Rpred <- predict(rb_c50rules, test2[,1:8])
table(rb_c50Rpred, test2$Diabetes)
confusionMatrix(rb_c50Rpred, test2$Diabetes)

datadiabet <- test2 %>% mutate(Diabetes = ifelse(Diabetes == "pos", 1, 0))
#Neural Net
library(neuralnet)
NN <- neuralnet(Diabetes ~.,data=train2, linear.output = FALSE
                , hidden = 5, rep = 10, threshold = 0.1)
plot(NN, rep = "best")
#predict
diabetpredNN<-neuralnet::compute(NN,test2[,1:8])
diabetpredNN
diabetresultNN <- data.frame(actual = datadiabet$Diabetes, 
                             prediction = diabetpredNN$net.result)
roundedresults<-sapply(diabetresultNN,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)
actualfactor <- as.factor(actual)
predfactor <- as.factor(prediction.2)
confusionMatrix(actualfactor,predfactor)

library(nnet)
nn1 <- nnet(Diabetes~., data=train2, size = 10, maxit = 300, trace=FALSE)
nn1pred <- predict(nn1, newdata = test2[,1:8], type = 'class')
pred1 <-  factor(nn1pred) 
table(pred1, test2$Diabetes)
confusionMatrix(pred1, test2$Diabetes)

#Compare Model
trainControl <- trainControl(method="cv", number=5)

fit.knn <- train(Diabetes~., data=train2, method="knn", metric="Accuracy", 
                 preProcess=c("center", "scale"), trControl=trainControl)
C45Fit <- train(Diabetes ~., method="J48", data=train2, metric="Accuracy", 
                trControl = trainControl)
fit.nb <- train(Diabetes~., data=train2, method="nb", metric="Accuracy", 
                trControl=trainControl)
fit.jrip <- train(Diabetes~., data=train2, method="JRip", metric="Accuracy", 
                  trControl=trainControl)
fit.oner <- train(Diabetes~., data=train2, method="OneR", metric="Accuracy", 
                  trControl=trainControl)

comparemdl <- resamples(list(C4.5=C45Fit, KNN=fit.knn, NB=fit.nb, JRip=fit.jrip, 
                             OneR=fit.oner))

summary(comparemdl)


#50 times Holdout
library(rminer)
full_accuracy_tree=0
list_acc <- list()
for(b in 1:50) # iterations
{
  H=holdout(data_clear$Diabetes,ratio=2/3,mode="random" ,seed=NULL)
  c45model <- J48(Diabetes~., data=data_clear[H$tr,])
  c45predict <- predict(c45model, data_clear[H$ts,-9] ,type="class")
  result <- confusionMatrix(c45predict, data_clear[H$ts,]$Diabetes)
  accuracy <- result$overall['Accuracy'] 
  cat("batch :",b,
      "accuracy:",accuracy,"\n")
  full_accuracy_tree= full_accuracy_tree+accuracy
  list_acc[[b]] <- accuracy
}
cat("Tree :",
    "accuracy:",full_accuracy_tree/50, "\n")

x <- c(1:50)
plot(x,list_acc,type = "o")

#K-fold Cross Validation
#Create 10 equally size folds
folds <- cut(seq(1,nrow(data_clear)),breaks=10,labels=FALSE)
full_accuracy_tree=0
list_acc <- list()
#Perform 10 fold cross validation
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data_clear[testIndexes, ]
  trainData <- data_clear[-testIndexes, ]
  c45model <- J48(Diabetes~., data=trainData)
  c45predict <- predict(c45model, testData[,-9] ,type="class")
  result <- confusionMatrix(c45predict, testData$Diabetes)
  accuracy <- result$overall['Accuracy'] 
  cat("batch :",i,
      "accuracy:",accuracy,"\n")
  full_accuracy_tree= full_accuracy_tree+accuracy
  list_acc[[i]] <- accuracy
}
cat("Tree :",
    "accuracy:",full_accuracy_tree/10, "\n")
x <- c(1:10)
plot(x,list_acc,type = "o")