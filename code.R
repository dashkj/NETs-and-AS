mydata <-read.csv(header=T,sep = ",",file.choose())#
mydata <-read.csv("C:/Users/huangjian/Desktop/IML/data.csv")#

sum(complete.cases(mydata)) #
is.na(mydata)#
sum(!complete.cases(mydata))#
mean(!complete.cases(mydata))  


mydata<-na.omit(mydata)#
summary(mydata)#


library(mice)#
library(lattice)
md.pattern(mydata)#
imp<-mice(mydata)#mice()
c3<-complete(imp,3)#
str(mydataMI)#
summary(mydataMI)#
table(mydataMI$dead)#



library(caret)
mydata <-read.csv("C:/Users/data.csv")#
imputation=preProcess(mydata,method = "medianImpute")
mydataMEI1=predict(imputation,mydata)
summary(mydataMEI1)#






library(caret)

set.seed(123)

trainIndex=createDataPartition(mydata$dead,p=.7,list = F,times = 1)
datTrain=mydata[trainIndex,]#
datTest=mydata[-trainIndex,]#


train<-mydata[ind==1,1:48]#
test<-mydata[ind==2,1:48]#



set.seed(9560)
down_train <- downSample(x = trainknn[,-48],
                         y = trainknn$dead)
table(down_train$Class) #

par(mfrow=c(1,3))#
boxplot(trainknn$aniongap,horizontal=F)#
boxplot(trainknn$bmi,horizontal=F)#
boxplot(trainknn$alt,horizontal=F)#
summary(trainknn)

#盖帽法
q1<-quantile(trainknn$bmi, 0.01)        #
q99<-quantile(trainknn$bmi, 0.99)       #replacement has 1 row, data has 0
trainknn[trainknn$bmi<q1,]$bmi<-q1
trainknn[trainknn$bmi>q99,]$bmi<-q99


q1<-quantile(trainknn$aniongap, 0.01)
q99<-quantile(trainknn$aniongap, 0.99)      
trainknn[trainknn$aniongap<q1,]$aniongap<-q1
trainknn[trainknn$aniongap>q99,]$aniongap<-q99

q99<-quantile(trainknn$bilirubin, 0.99)      
trainknn[trainknn$bilirubin>q99,]$bilirubin<-q99

q99<-quantile(trainknn$alt, 0.99)      
trainknn[trainknn$alt>q99,]$alt<-q99

q99<-quantile(trainknn$ast, 0.99)      
trainknn[trainknn$ast>q99,]$ast<-q99

q99<-quantile(trainknn$alp, 0.99)      
trainknn[trainknn$alp>q99,]$alp<-q99

q1<-quantile(trainknn$bun, 0.01)
q99<-quantile(trainknn$bun, 0.99)      
trainknn[trainknn$bun<q1,]$bun<-q1
trainknn[trainknn$bun>q99,]$bun<-q99

q1<-quantile(trainknn$creatinine, 0.01)
q99<-quantile(trainknn$creatinine, 0.99)      
trainknn[trainknn$creatinine<q1,]$creatinine<-q1
trainknn[trainknn$creatinine>q99,]$creatinine<-q99

q1<-quantile(trainknn$calcium, 0.01)
q99<-quantile(trainknn$calcium, 0.99)      
trainknn[trainknn$calcium<q1,]$calcium<-q1
trainknn[trainknn$calcium>q99,]$calcium<-q99

q1<-quantile(trainknn$glucose, 0.01)
q99<-quantile(trainknn$glucose, 0.99)      
trainknn[trainknn$glucose<q1,]$glucose<-q1
trainknn[trainknn$glucose>q99,]$glucose<-q99

q1<-quantile(trainknn$bicarbonate, 0.01)
q99<-quantile(trainknn$bicarbonate, 0.99)      
trainknn[trainknn$bicarbonate<q1,]$bicarbonate<-q1
trainknn[trainknn$bicarbonate>q99,]$bicarbonate<-q99

q1<-quantile(trainknn$wbc, 0.01)
q99<-quantile(trainknn$wbc, 0.99)      
trainknn[trainknn$wbc<q1,]$wbc<-q1
trainknn[trainknn$wbc>q99,]$wbc<-q99

q1<-quantile(trainknn$hematocrit, 0.01)
q99<-quantile(trainknn$hematocrit, 0.99)      
trainknn[trainknn$hematocrit<q1,]$hematocrit<-q1
trainknn[trainknn$hematocrit>q99,]$hematocrit<-q99

q1<-quantile(trainknn$hemoglobin, 0.01)
q99<-quantile(trainknn$hemoglobin, 0.99)      
trainknn[trainknn$hemoglobin<q1,]$hemoglobin<-q1
trainknn[trainknn$hemoglobin>q99,]$hemoglobin<-q99

q1<-quantile(trainknn$platelets, 0.01)        
q99<-quantile(trainknn$platelets, 0.99)       
trainknn[trainknn$platelets<q1,]$platelets<-q1
trainknn[trainknn$platelets>q99,]$platelets<-q99

q1<-quantile(trainknn$inr, 0.01)
q99<-quantile(trainknn$inr, 0.99)      
trainknn[trainknn$inr<q1,]$inr<-q1
trainknn[trainknn$inr>q99,]$inr<-q99

q1<-quantile(trainknn$rbc, 0.01)
q99<-quantile(trainknn$rbc, 0.99)      
trainknn[trainknn$rbc<q1,]$rbc<-q1
trainknn[trainknn$rbc>q99,]$rbc<-q99


q1<-quantile(trainknn$mcv, 0.01)
q99<-quantile(trainknn$mcv, 0.99)      
trainknn[trainknn$mcv<q1,]$mcv<-q1
trainknn[trainknn$mcv>q99,]$mcv<-q99

q1<-quantile(trainknn$mchc, 0.01)
q99<-quantile(trainknn$mchc, 0.99)      
trainknn[trainknn$mchc<q1,]$mchc<-q1
trainknn[trainknn$mchc>q99,]$mchc<-q99

q1<-quantile(trainknn$mch, 0.01)
q99<-quantile(trainknn$mch, 0.99)      
trainknn[trainknn$mch<q1,]$mch<-q1
trainknn[trainknn$mch>q99,]$mch<-q99

q1<-quantile(trainknn$PT, 0.01)
q99<-quantile(trainknn$PT, 0.99)      
trainknn[trainknn$PT<q1,]$PT<-q1
trainknn[trainknn$PT>q99,]$PT<-q99

q1<-quantile(trainknn$HR, 0.01)
q99<-quantile(trainknn$HR, 0.99)      
trainknn[trainknn$HR<q1,]$HR<-q1
trainknn[trainknn$HR>q99,]$HR<-q99

q1<-quantile(trainknn$RR, 0.01)
q99<-quantile(trainknn$RR, 0.99)      
trainknn[trainknn$RR<q1,]$RR<-q1
trainknn[trainknn$RR>q99,]$RR<-q99

q1<-quantile(trainknn$spo2, 0.01)
trainknn[trainknn$spo2<q1,]$spo2<-q1



summary(trainknn)
#test
summary(testknn)
q1<-quantile(testknn$bmi, 0.01)        #
q99<-quantile(testknn$bmi, 0.99)       #replacement has 1 row, data has 0 
testknn[testknn$bmi<q1,]$bmi<-q1
testknn[testknn$bmi>q99,]$bmi<-q99


q1<-quantile(testknn$aniongap, 0.01)
q99<-quantile(testknn$aniongap, 0.99)      
testknn[testknn$aniongap<q1,]$aniongap<-q1
testknn[testknn$aniongap>q99,]$aniongap<-q99

q99<-quantile(testknn$bilirubin, 0.99)      
testknn[testknn$bilirubin>q99,]$bilirubin<-q99

q99<-quantile(testknn$alt, 0.99)      
testknn[testknn$alt>q99,]$alt<-q99

q99<-quantile(testknn$ast, 0.99)      
testknn[testknn$ast>q99,]$ast<-q99

q99<-quantile(testknn$alp, 0.99)      
testknn[testknn$alp>q99,]$alp<-q99

q1<-quantile(testknn$bun, 0.01)
q99<-quantile(testknn$bun, 0.99)      
testknn[testknn$bun<q1,]$bun<-q1
testknn[testknn$bun>q99,]$bun<-q99

q1<-quantile(testknn$creatinine, 0.01)
q99<-quantile(testknn$creatinine, 0.99)      
testknn[testknn$creatinine<q1,]$creatinine<-q1
testknn[testknn$creatinine>q99,]$creatinine<-q99

q1<-quantile(testknn$calcium, 0.01)
q99<-quantile(testknn$calcium, 0.99)      
testknn[testknn$calcium<q1,]$calcium<-q1
testknn[testknn$calcium>q99,]$calcium<-q99

q1<-quantile(testknn$glucose, 0.01)
q99<-quantile(testknn$glucose, 0.99)      
testknn[testknn$glucose<q1,]$glucose<-q1
testknn[testknn$glucose>q99,]$glucose<-q99

q1<-quantile(testknn$bicarbonate, 0.01)
q99<-quantile(testknn$bicarbonate, 0.99)      
testknn[testknn$bicarbonate<q1,]$bicarbonate<-q1
testknn[testknn$bicarbonate>q99,]$bicarbonate<-q99

q1<-quantile(testknn$wbc, 0.01)
q99<-quantile(testknn$wbc, 0.99)      
testknn[testknn$wbc<q1,]$wbc<-q1
testknn[testknn$wbc>q99,]$wbc<-q99

q1<-quantile(testknn$hematocrit, 0.01)
q99<-quantile(testknn$hematocrit, 0.99)      
testknn[testknn$hematocrit<q1,]$hematocrit<-q1
testknn[testknn$hematocrit>q99,]$hematocrit<-q99

q1<-quantile(testknn$hemoglobin, 0.01)
q99<-quantile(testknn$hemoglobin, 0.99)      
testknn[testknn$hemoglobin<q1,]$hemoglobin<-q1
testknn[testknn$hemoglobin>q99,]$hemoglobin<-q99

q1<-quantile(testknn$platelets, 0.01)        
q99<-quantile(testknn$platelets, 0.99)       
testknn[testknn$platelets<q1,]$platelets<-q1
testknn[testknn$platelets>q99,]$platelets<-q99

q1<-quantile(testknn$inr, 0.01)
q99<-quantile(testknn$inr, 0.99)      
testknn[testknn$inr<q1,]$inr<-q1
testknn[testknn$inr>q99,]$inr<-q99

q1<-quantile(testknn$rbc, 0.01)
q99<-quantile(testknn$rbc, 0.99)      
testknn[testknn$rbc<q1,]$rbc<-q1
testknn[testknn$rbc>q99,]$rbc<-q99


q1<-quantile(testknn$mcv, 0.01)
q99<-quantile(testknn$mcv, 0.99)      
testknn[testknn$mcv<q1,]$mcv<-q1
testknn[testknn$mcv>q99,]$mcv<-q99

q1<-quantile(testknn$mchc, 0.01)
q99<-quantile(testknn$mchc, 0.99)      
testknn[testknn$mchc<q1,]$mchc<-q1
testknn[testknn$mchc>q99,]$mchc<-q99

q1<-quantile(testknn$mch, 0.01)
q99<-quantile(testknn$mch, 0.99)      
testknn[testknn$mch<q1,]$mch<-q1
testknn[testknn$mch>q99,]$mch<-q99

q1<-quantile(testknn$PT, 0.01)
q99<-quantile(testknn$PT, 0.99)      
testknn[testknn$PT<q1,]$PT<-q1
testknn[testknn$PT>q99,]$PT<-q99

q1<-quantile(testknn$HR, 0.01)
q99<-quantile(testknn$HR, 0.99)      
testknn[testknn$HR<q1,]$HR<-q1
testknn[testknn$HR>q99,]$HR<-q99

q1<-quantile(testknn$RR, 0.01)
q99<-quantile(testknn$RR, 0.99)      
testknn[testknn$RR<q1,]$RR<-q1
testknn[testknn$RR>q99,]$RR<-q99

q1<-quantile(testknn$spo2, 0.01)
testknn[testknn$spo2<q1,]$spo2<-q1



summary(testknn)



library(rrtable)
library(officer)
table2docx(tab1)
table2docx(tab2)




train<-trainknn[,c(1:44,48)]
test<-testknn[,c(1:44,48)]
#IML
library(caret)
library(doParallel)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)
## When you are done:
stopCluster(cl)


control <- rfeControl(functions=caretFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(train[,c(1:44)], train[,45], sizes=c(5,10,15), rfeControl=control,
               method = "rf")#Adaboost.M1,rf,svmRadial，


# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("o", "p"), col = "#3cc08f")
xyplot(results$results$Accuracy  ~
         results$results$Variables,
       type = c("g", "p", "l"),
       auto.key = TRUE)


library(caret)
library(pROC)
library(ggplot2)

names(train)
train<-train[,c(19,9,31,35,1,10,17,22,45)]
test<-test[,c(19,9,31,35,1,10,17,22,45)]
str(train)
train$dead<-factor(train$dead,levels=c(0,1),labels=c("no","yes"))
test$dead<-factor(test$dead,levels=c(0,1),labels=c("no","yes"))

ctrl <- trainControl(method = "repeatedcv",#
                     number = 3,#
                     
                     repeats = 1,
                     
                     verboseIter = T, 
                     classProbs = T,
                     returnData = F,
                     
                     p = 0.70,
                     summaryFunction = twoClassSummary,
                     allowParallel = T,
                     search = "random",#
                     selectionFunction = "tolerance")

names(getModelInfo())

set.seed(123)
fit.svm <- train(dead ~ .,data=train,
                 method = "svmRadial", #
                 trControl = ctrl, 
                 preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")


set.seed(123)
fit.nn <- train(dead~., data = train, method="nnet",metric = "ROC",trControl = ctrl,preProc = c("center", "scale"), 
                tuneLength = 4)
set.seed(123)
fit.mlp <- train(dead~., data = train, method="mlp",metric = "ROC",trControl = ctrl,preProc = c("center", "scale"), 
                 tuneLength = 4)
set.seed(123)
fit.GP <- train(dead~., data = train, method="gaussprRadial", metric = "ROC",trControl = ctrl, 
                       tuneLength = 4)

set.seed(123)
fit.gbm <- train(dead~., data = train, method="gbm", metric = "ROC",trControl = ctrl, 
                tuneLength = 4)

set.seed(123)
fit.LR <- train(dead~., data = train, method="glm", metric = "ROC",trControl = ctrl, 
                tuneLength = 4)
set.seed(123)
fit.nb <- train(dead~., data = train, method="naive_bayes", metric = "ROC",trControl = ctrl, 
                tuneLength = 4)
set.seed(123)
fit.adaboost <- train(dead~., data = train, method="AdaBoost.M1", metric = "ROC",trControl = ctrl, 
                      tuneLength = 4)#

set.seed(123)
fit.xgb <- train(dead ~ ., data=train, 
                 method = "xgbTree",
                 trControl = ctrl, 
                 tuneLength = 8,
                 metric = "ROC")
set.seed(123)
fit.rf <- train(dead ~ ., data=train, 
                method = "rf", #ordinalRF,ranger
                tuneGrid = fit.rf$bestTune,
                trControl = ctrl, 
                tuneLength = 8,
                metric = "ROC")

set.seed(123)
fit.C5.0 <- train(dead ~ ., data=train, 
                       method = "C5.0",
                       trControl = ctrl, 
                       tuneLength = 1,
                       metric = "ROC")
set.seed(123)
fit.knn <- train(dead ~ ., data=train, 
                 method = "kknn",preProc = c("center", "scale"), 
                 trControl = ctrl, 
                 tuneLength = 4,
                 metric = "ROC")



pred.final_train<- predict(fit.LR,newdata=train)

df<-confusionMatrix(pred.final_train,train$dead,positive = "yes")
df$byClass
df$overall
confusionMatrix(pred.final_train,train$dead,positive = "yes")
fourfoldplot(df$table, color = c("cyan", "pink"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
#95%CI
library(epiR)
cft<-c(12,9,184,1295)#TP,FP,FN,TN
epi.tests(cft)

pred.final_test <- predict(fit.LR,newdata=test)
confusionMatrix(pred.final_test,test$dead,positive = "yes")

df<-confusionMatrix(pred.final_test,test$dead,positive = "yes")
df$byClass
df$overall
confusionMatrix(pred.final,test$dead,positive = "yes")
#95%CI
cft<-c(5,2,53,314)#TP,FP,FN,TN
epi.tests(cft)


library(pROC)
library(ggplot2)

nn.probs = predict(fit.nn,train[,!names(train) %in% c("dead")],type = "prob")
knn.probs = predict(fit.knn,train[,!names(train) %in% c("dead")],type = "prob")
rf.probs = predict(fit.rf,train[,!names(train) %in% c("dead")],type = "prob")
adaboost.probs = predict(fit.adaboost,train[,!names(train) %in% c("dead")],type = "prob")
xgb.probs = predict(fit.xgb,train[,!names(train) %in% c("dead")],type = "prob")
C5.0.probs = predict(fit.C5.0,train[,!names(train) %in% c("dead")],type = "prob")
svm.probs = predict(fit.svm,train[,!names(train) %in% c("dead")],type = "prob")
GP.probs = predict(fit.GP,train[,!names(train) %in% c("dead")],type = "prob")
LR.probs = predict(fit.LR,train[,!names(train) %in% c("dead")],type = "prob")
nb.probs = predict(fit.nb,train[,!names(train) %in% c("dead")],type = "prob")
GBM.probs = predict(fit.gbm,train[,!names(train) %in% c("dead")],type = "prob")
mlp.probs = predict(fit.mlp,train[,!names(train) %in% c("dead")],type = "prob")


adaboost.ROC = roc(response = train[,c("dead")],
                   predictor = adaboost.probs$yes,
                   levels = levels(test[,c("dead")]))
plot(adaboost.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#
xgb.ROC = roc(response = train[,c("dead")],
              predictor = xgb.probs$yes,
              levels = levels(test[,c("dead")]))
plot(xgb.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#
GP.ROC = roc(response = train[,c("dead")],
              predictor = GP.probs$yes,
              levels = levels(test[,c("dead")]))
plot(GP.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#
C5.0.ROC = roc(response = train[,c("dead")],
             predictor = C5.0.probs$yes,
             levels = levels(test[,c("dead")]))
plot(C5.0.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#
svm.ROC = roc(response = train[,c("dead")],
              predictor = svm.probs$yes,
              levels = levels(test[,c("dead")]))
plot(svm.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#
gbm.ROC = roc(response = train[,c("dead")],
              predictor = GBM.probs$yes,
              levels = levels(test[,c("dead")]))
plot(gbm.ROC,col = "purple",legacy.axes=T,type = "S",print.auc=T)#
LR.ROC = roc(response = train[,c("dead")],
             predictor = LR.probs$yes,
             levels = levels(test[,c("dead")]))
plot(LR.ROC,col = "red",legacy.axes=T,type = "S",print.auc=T)#
nb.ROC = roc(response = train[,c("dead")],
             predictor = nb.probs$yes,
             levels = levels(test[,c("dead")]))
plot(nb.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#
knn.ROC = roc(response = train[,c("dead")],
              predictor = knn.probs$yes,
              levels = levels(test[,c("dead")]))
plot(knn.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#

nn.ROC = roc(response = train[,c("dead")],
             predictor = nn.probs$yes,
             levels = levels(test[,c("dead")]))
plot(nn.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#
mlp.ROC = roc(response = train[,c("dead")],
              predictor = mlp.probs$yes,
              levels = levels(test[,c("dead")]))
plot(mlp.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#
rf.ROC = roc(response = train[,c("dead")],
             predictor = rf.probs$yes,
             levels = levels(test[,c("dead")]))
plot(rf.ROC,col = "red",legacy.axes=T,type = "S",print.auc=T)#


test.knn.probs = predict(fit.knn,test[,!names(test) %in% c("dead")],type = "prob")
test.nn.probs = predict(fit.nn,test[,!names(test) %in% c("dead")],type = "prob")
test.mlp.probs = predict(fit.mlp,test[,!names(test) %in% c("dead")],type = "prob")
test.svm.probs = predict(fit.svm,test[,!names(test) %in% c("dead")],type = "prob")
test.LR.probs = predict(fit.LR,test[,!names(test) %in% c("dead")],type = "prob")
test.nb.probs = predict(fit.nb,test[,!names(test) %in% c("dead")],type = "prob")
test.GBM.probs = predict(fit.gbm,test[,!names(test) %in% c("dead")],type = "prob")
test.GP.probs = predict(fit.GP,test[,!names(test) %in% c("dead")],type = "prob")
test.adaboost.probs= predict(fit.adaboost,test[,!names(test) %in% c("dead")],type = "prob")
test.xgb.probs = predict(fit.xgb,test[,!names(test) %in% c("dead")],type = "prob")
test.C5.0.probs = predict(fit.C5.0,test[,!names(test) %in% c("dead")],type = "prob")
test.rf.probs = predict(fit.rf,test[,!names(test) %in% c("dead")],type = "prob")

test.LR.ROC = roc(response = test[,c("dead")],
                  predictor = test.LR.probs$yes,
                  levels = levels(test[,c("dead")]))
plot(test.LR.ROC,col = "red",legacy.axes=T,type = "S",print.auc=T)#

test.nb.ROC = roc(response = test[,c("dead")],
                  predictor = test.nb.probs$yes,
                  levels = levels(train[,c("dead")]))
plot(test.nb.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#

test.gbm.ROC = roc(response = test[,c("dead")],
                   predictor = test.GBM.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.gbm.ROC,col = "purple",legacy.axes=T,type = "S",print.auc=T)#

test.GP.ROC = roc(response = test[,c("dead")],
                   predictor = test.GP.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.GP.ROC,col = "purple",legacy.axes=T,type = "S",print.auc=T)#

test.svm.ROC = roc(response = test[,c("dead")],
                   predictor = test.svm.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.svm.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#

test.adaboost.ROC = roc(response = test[,c("dead")],
                        predictor = test.adaboost.probs$yes,
                        levels = levels(train[,c("dead")]))
plot(test.adaboost.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#

test.xgb.ROC = roc(response = test[,c("dead")],
                   predictor = test.xgb.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.xgb.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#

test.C5.0.ROC = roc(response = test[,c("dead")],
                   predictor = test.C5.0.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.C5.0.ROC,col = "green",legacy.axes=T,type = "S",print.auc=T)#

test.knn.ROC = roc(response = test[,c("dead")],
                   predictor = test.knn.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.knn.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#

test.nn.ROC = roc(response = test[,c("dead")],
                  predictor = test.nn.probs$yes,
                  levels = levels(train[,c("dead")]))
plot(test.nn.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#

test.mlp.ROC = roc(response = test[,c("dead")],
                   predictor = test.mlp.probs$yes,
                   levels = levels(train[,c("dead")]))
plot(test.mlp.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#

test.rf.ROC = roc(response = test[,c("dead")],
                  predictor = test.rf.probs$yes,
                  levels = levels(train[,c("dead")]))
plot(test.rf.ROC,col = "blue",legacy.axes=T,type = "S",print.auc=T)#




set.seed(123)
fit.svm <- train(dead ~ ., data=train,
                 trControl = svm_trcontrol_1,
                 tuneGrid = svm_grid_1, 
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 method = "svmRadial",
                 nthread = 8)
fit.svm
#sigma =  and C = 
fit.svm <- train(dead ~ .,data=train,
                 method = "svmRadial", tuneGrid = fit.svm$bestTune,#
                 preProc = c("center", "scale"),metric = "ROC",trControl = svm_trcontrol_1)
fit.svm
fit.svm%>%test_roc(data = test)%>%auc()



#
library(ggplot2)
library(predtools)
#
train$dead<-as.numeric(train$dead)#
train$dead<-ifelse(train$dead>1,1,0)#
##
train$pred <-xgb.probs$yes
calibration_plot(data = train, obs = "dead", pred = "pred", title = "Calibration plot", y_lim = c(0, 1),x_lim = c(0, 0.8))
#
test$dead<-as.numeric(test$dead)
test$dead<-ifelse(test$dead>1,1,0)
test$pred <-test.xgb.probs$yes
calibration_plot(data = test, obs = "dead", pred = "pred", title = "Calibration plot", y_lim = c(0, 1))



#DCA
dca <- function(data, outcome, predictors, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  # data MUST BE A DATA FRAME
  if (class(data)!="data.frame") {
    stop("Input data must be class data.frame")
  }
  
  #ONLY KEEPING COMPLETE CASES
  data=data[complete.cases(data[append(outcome,predictors)]),append(outcome,predictors)]
  
  # outcome MUST BE CODED AS 0 AND 1
  if (max(data[[outcome]])>1 | min(data[[outcome]])<0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  if (xstart<0 | xstart>1) {
    stop("xstart must lie between 0 and 1")
  }
  
  # xstop IS BETWEEN 0 AND 1
  if (xstop<0 | xstop>1) {
    stop("xstop must lie between 0 and 1")
  }
  
  # xby IS BETWEEN 0 AND 1
  if (xby<=0 | xby>=1) {
    stop("xby must lie between 0 and 1")
  }
  
  # xstart IS BEFORE xstop
  if (xstart>=xstop) {
    stop("xstop must be larger than xstart")
  }
  
  #STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n=length(predictors)
  
  #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
  if (length(probability)>0 & pred.n!=length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  
  #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm)>0 & pred.n!=length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm)==0) {
    harm=rep(0,pred.n)
  }
  if (length(probability)==0) {
    probability=rep(TRUE,pred.n)
  }
  
  
  #CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO, 
  #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  #IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  for(m in 1:pred.n) { 
    if (probability[m]!=TRUE & probability[m]!=FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
      stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))  
    }
    if(probability[m]==FALSE) {
      model=NULL
      pred=NULL
      model=glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family=binomial("logit"))
      pred=data.frame(model$fitted.values)
      pred=data.frame(pred)
      names(pred)=predictors[m]
      data=cbind(data[names(data)!=predictors[m]],pred)
      print(paste(predictors[m],"converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.",sep=" "))
    }
  }
  
  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors=="all" | predictors=="none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }  
  
  #########  CALCULATING NET BENEFIT   #########
  N=dim(data)[1]
  event.rate=colMeans(data[outcome])
  
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb=data.frame(seq(from=xstart, to=xstop, by=xby))
  names(nb)="threshold"
  interv=nb
  
  nb["all"]=event.rate - (1-event.rate)*nb$threshold/(1-nb$threshold)
  nb["none"]=0
  
  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for(m in 1:pred.n){
    for(t in 1:length(nb$threshold)){
      # COUNTING TRUE POSITIVES AT EACH THRESHOLD
      tp=mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome])*sum(data[[predictors[m]]]>=nb$threshold[t])
      # COUNTING FALSE POSITIVES AT EACH THRESHOLD
      fp=(1-mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome]))*sum(data[[predictors[m]]]>=nb$threshold[t])
      #setting TP and FP to 0 if no observations meet threshold prob.
      if (sum(data[[predictors[m]]]>=nb$threshold[t])==0) {
        tp=0
        fp=0
      }
      
      # CALCULATING NET BENEFIT
      nb[t,predictors[m]]=tp/N - fp/N*(nb$threshold[t]/(1-nb$threshold[t])) - harm[m]
    }
    interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
  }
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  for(m in 1:pred.n) {
    if (smooth==TRUE){
      lws=loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
      
      lws=loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
    }
  }
  
  # PLOTTING GRAPH IF REQUESTED
  if (graph==TRUE) {
    require(graphics)
    
    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if(intervention==TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      
      #getting maximum number of avoided interventions
      ymax=max(interv[predictors],na.rm = TRUE)
      
      #INITIALIZING EMPTY PLOT WITH LABELS
      plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
      
      #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
        } else {
          lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
        }
        
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      
      #getting maximum net benefit
      ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
      
      # inializing new benfit plot with treat all option
      plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
      # adding treat none option
      lines(x=nb$threshold, y=nb$none,lwd=2)
      #PLOTTING net benefit FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=m,lty=2) 
        } else {
          lines(nb$threshold,data.matrix(nb[predictors[m]]),col=m,lty=2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
    
  }
  
  #RETURNING RESULTS
  results=list() 
  results$N=N
  results$predictors=data.frame(cbind(predictors,harm,probability))
  names(results$predictors)=c("predictor","harm.applied","probability")
  results$interventions.avoided.per=interventionper
  results$net.benefit=nb
  results$interventions.avoided=interv
  
  return(results)
  
}  

dca(data=train,outcome = "dead",predictors = "pred",probability = T,xstop = 1.0)
dca(data=test,outcome = "dead",predictors = "pred",probability = T,xstop = 0.9)

