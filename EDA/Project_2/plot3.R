#Exploratory Data Analysis Course Project 2 plot2
library(ggplot2)
library(reshape)
#read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#calcuate total emissions by year
totals<-melt(tapply(NEI[NEI$fips=="24510",4],
               list(as.factor(NEI[NEI$fips=="24510",6]), 
                    as.factor(NEI[NEI$fips=="24510",5])), sum))

transform(totals, X2=factor(X2))
png("plot3.png", width=600, height=600)

qplot(X1, value, data=totals, color=X2, geom="line",xlab = "Year", ylab="Total Emissions")

dev.off()
