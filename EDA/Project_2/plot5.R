#Exploratory Data Analysis Course Project 2 plot5

#read in the data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

Ids<-SCC[grep("Vehicles", SCC$EI.Sector),1]

#calcuate total emissions by year
totals<-tapply(NEI[NEI$fips=="24510" & NEI$SCC %in% Ids,4], as.factor(NEI[NEI$fips=="24510" & NEI$SCC %in% Ids,6]), sum)

png("plot5.png", width=600, height=600)
plot(c(1999,2002,2005,2008),totals, type="l", main="PM2.5 from Vehicles in Baltimore City", lwd=2, xlab="Year", ylab="Total Emissions")
dev.off()
