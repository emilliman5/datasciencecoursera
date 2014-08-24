#Exploratory Data Analysis Course Project 2 plot1

#read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#calcuate total emissions by year
totals<-tapply(NEI$Emissions, as.factor(NEI$year), sum)

png("plot1.png", width=600, height=600)
plot(c(1999,2002,2005,2008),totals, type="h", lwd=10, xlab="Year", ylab="Total Emissions", main = "Total PM2.5 Emissions")
dev.off()
