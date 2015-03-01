library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
library(caret)

vowel.test$y<-factor(vowel.test$y)
vowel.train$y<-factor(vowel.train$y)
set.seed(33833)

modRF<-train(y~., data=vowel.train, method="rf")
modGBM<-train(y~., data=vowel.train, method="gbm")

testRF<-predict(object = modRF, newdata = vowel.test)
testGBM<-predict(modGBM, vowel.test)

confusionMatrix(testRF,vowel.test$y,)
confusionMatrix(testGBM,vowel.test$y)

df<-data.frame(testGBM==testRF,testGBM==vowel.test$y, testRF==vowel.test$y, vowel.test$y)

sum(testGBM==testRF)/length(df[,3])
sum(testGBM==vowel.test$y)/length(df[,3])
sum(testRF==vowel.test$y)/length(df[,3])

sum(df[df[,2]==TRUE,3])/length(df[,3])
sum(df[df[,3]==TRUE & df[,2]==TRUE & df[,1]==TRUE,3])/length(df[df[,1]==TRUE,1])


##Q2
library(caret)
library(gbm)
set.seed(62433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
head(testing)

modRF<-train(diagnosis~.,training,method="rf")
modGBM<-train(diagnosis~.,training,method="gbm", verbose=FALSE)
modLDA<-train(diagnosis~.,training,method="lda")

predRF<-predict(modRF, training)
predGBM<-predict(modGBM,training)
predLDA<-predict(modLDA,training)

stackDF<-data.frame(predRF,predGBM,predLDA, diagnosis=training$diagnosis)

modStack<-train(diagnosis~., testPredDF, method="rf")


testRF<-predict(modRF,testing)
testGBM<-predict(modGBM,testing)
testLDA<-predict(modLDA, testing)
testPredDF<-data.frame(testRF,testGBM,testLDA, diagnosis=testing$diagnosis)
testStack<-predict(modStack,testPredDF)
confusionMatrix(testRF,testing$diagnosis)$overall[1]
confusionMatrix(testGBM,testing$diagnosis)$overall[1]
confusionMatrix(testLDA,testing$diagnosis)$overall[1]
confusionMatrix(testStack,testing$diagnosis)$overall[1]


### Q3

set.seed(233)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

modlasso<-enet(as.matrix(training[,-9]),training$CompressiveStrength)
plot.enet(modlasso, xvar="penalty")

##Q4 

library(lubridate)  # For year() function below
library(forecast)
dat = read.csv("~/Downloads/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest<-ts(testing$visitsTumblr, start=366)

q4mod<-bats(tstrain)
predQ4<-forecast(q5mod,h=235, level=95)
accuracy(predQ4,tstest)
plot(predQ4)
lines(tstest, col="red")

length(subset(tstest,subset = tstest>=predQ4$lower & tstest<=predQ4$upper))/length(tstest)
##Q5

set.seed(325)
library(e1071)
library(AppliedPredictiveModeling)
library(hydroGOF)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

modSVM<-svm(CompressiveStrength~., training)
testSVM<-predict(modSVM,testing)
testSVM
rmse(testSVM,testing$CompressiveStrength)
