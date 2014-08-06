getwd()
setwd("~/Documents/GitHub_Repos/datasciencecoursera/gettingAndCleaningData/")

q1_data<-read.csv("getdata-data-ss06hid.csv")

strsplit(colnames(q1_data), "wgtp")[[123]]

q2_raw<-read.csv("getdata-data-GDP.csv")
head(q2)
tail(q2)
q2<-q2[1:190,]
q2<-q2[as.numeric(q2$Gross.domestic.product.2012)>=1,]
GDP<-gsub(",", replacement = "",x = q2$X.3[])


GDP<-GDP[4:length(GDP)]
mean(as.numeric(GDP))

grep("^United", q2_raw$X.2)

edu<-read.csv("getdata-data-EDSTATS_Country.csv")
head(edu)

final_edu<-edu[match(as.character(q2$X),table = as.character(edu$CountryCode)),]
head(final_edu)
as.list(final_edu[1,])

grep("Fiscal year end: Jun", final_edu$Special.Notes, value=TRUE)



library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

head(sampleTimes)
class(sampleTimes)
length(sampleTimes[sampleTimes>=as.Date("2012-01-01", "%Y-%m-%d")  & 
              sampleTimes<=as.Date("2012-12-31","%Y-%m-%d")])
AMZN_2012<-sampleTimes[sampleTimes>=as.Date("2012-01-01", "%Y-%m-%d")  & 
                         sampleTimes<=as.Date("2012-12-31","%Y-%m-%d")]
summary(as.factor(weekdays(AMZN_2012)))
