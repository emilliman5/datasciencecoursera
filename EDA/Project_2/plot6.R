#Exploratory Data Analysis Course Project 2 plot6
library(ggplot2)
library(reshape2)
#read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#find SCC Ids for motor vehicles (Mobile, vehicles, I exculded non-road)
Ids<-SCC[grep("Vehicles", SCC$EI.Sector),1]

#calcuate total emissions by year and locations
totals<-tapply(NEI[(NEI$fips=="24510" | NEI$fips=="06037")& NEI$SCC %in% Ids,4], 
               list(as.factor(NEI[(NEI$fips=="24510" | NEI$fips=="06037") & NEI$SCC %in% Ids,6]), 
                    as.factor(NEI[(NEI$fips=="24510" | NEI$fips=="06037") & NEI$SCC %in% Ids,1])), 
                sum)

#transform the data to tall and skinny for easy factoring
emissions<-melt(totals)

png("plot6.png", width=800, height=800)
qplot(X1, value, data=emissions, color= as.factor(X2), geom="line", main="Vehicle emissions: Baltimore vs. LA", aes(size=1), xlab="Year", ylab="Total Emissions")
dev.off()
