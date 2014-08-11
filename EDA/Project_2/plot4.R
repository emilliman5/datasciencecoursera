#Exploratory Data Analysis Course Project 2 plot2
library(ggplot2)
#read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#search for Location IDs that are marked as Coal combusting sources
#The EI sector variable is a controlled vocabulary variable with three entries for 
#Combustion and Coal as fuel source.

Ids<-SCC[grep("[Cc]oal", SCC$EI.Sector),1]

#calcuate total emissions by year for all entries in the Ids vector (Coal combustion)
totals<-tapply(NEI[NEI$SCC %in% Ids,4],
               list(as.factor(NEI[NEI$SCC %in% Ids,6])), sum)

#make the desired plot to determine how emissions from coal combustion has changed.
png("plot4.png", width=600, height=600)
plot(c(1999,2002,2005,2008), totals, type="l", main="PM2.5 from coal combustion sources", lwd=2, xlab="Year", ylab="Total Emissions")
dev.off()
