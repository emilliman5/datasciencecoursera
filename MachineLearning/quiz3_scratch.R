library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
head(trainSA)
modFit<-train(chd~age+alcohol+obesity+ldl+typea+tobacco, method="glm",family="binomial", data=trainSA)

predictTest<-predict(modFit, newdata=testSA)
predictTrain<-predict(modFit, newdata=trainSA)

missClass(testSA$chd,predictTest)
missClass(trainSA$chd,predictTrain)


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)

modFit<-train(y~., data=rbind(vowel.train,vowel.test), method="rf")
varImp(modFit)
modFit2<-train(y~., data=vowel.train, method="rf")
varImp(modFit2)
