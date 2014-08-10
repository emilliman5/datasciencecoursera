#Exploratory Data Analysis Course Project 2 plot2

#read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#calcuate total emissions by year
totals<-tapply(NEI[NEI$fips=="24510",4], as.factor(NEI[NEI$fips=="24510",6]), sum)

png("plot2.png", width=600, height=600)
plot(c(1999,2002,2005,2008),totals, type="h", main="PM2.5 Baltimore City", lwd=10, xlab="Year", ylab="Total Emissions")
dev.off()