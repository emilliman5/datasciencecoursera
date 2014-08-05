q1URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(q1URL, destfile = "q1data.csv", method="curl")

q1Data<-read.csv("q1data.csv")

q1Data$agricultureLogical<-ifelse((q1Data$ACR>=3 & q1Data$AGS>=6), TRUE, FALSE)

head(q1Data)

library(jpeg)

q2URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg "
download.file(q2URL, destfile = "q2.jpeg", method="curl")

q2<-readJPEG("q2.jpeg", native=TRUE)

quantile(q2, probs=c(.3,.8))

q3GDPURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
q3eduURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(q3eduURL, destfile = "q3Edu.csv", method="curl")
download.file(q3GDPURL, destfile = "q3GDP.csv",method = "curl")

q3GDP<-read.csv("q3GDP.csv", stringsAsFactors=FALSE)
q3Edu<-read.csv("q3Edu.csv", stringsAsFactors=FALSE)

head(q3Edu)
head(q3GDP)

dim(q3Edu)
dim(q3GDP)

class(q3GDP$Gross.domestic.product.2012)
colnames(q3GDP)

q3GDP<-q3GDP[q3GDP$Gross.domestic.product.2012>=1,]

q3GDP[,2]

q3GDP<-q3GDP[2:191,]

mergedData<-merge(q3Edu, q3GDP, by.x = "CountryCode", by.y = "X")

head(mergedData)

colnames(mergedData)
mergedData[order(as.numeric(mergedData$Gross.domestic.product.2012), decreasing = TRUE)[13],]

summary(mergedData$Gross.domestic.product.2012)
mergedData$Gross.domestic.product.2012<-as.numeric(mergedData$Gross.domestic.product.2012)
mergedData$X.3<-gsub(",","", mergedData$X.3)
mergedData$X.3<-as.numeric(mergedData$X.3)

mergedData$Income.Group<-as.factor(mergedData$Income.Group)
summary(mergedData$Income.Group)
tapply(mergedData$Gross.domestic.product.2012, INDEX=mergedData$Income.Group, FUN=mean)

quantile(mergedData$Gross.domestic.product.2012) ~ mergedData$Income.Group

mergedData$GDPGroup<-cut(mergedData$Gross.domestic.product.2012,breaks = 5)

table(mergedData$GDPGroup, mergedData$Income.Group)
