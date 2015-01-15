NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

### Draw plot 1 for total emissions from PM2.5 in the United States from 1999 to 2008
aggTotals <- aggregate(Emissions ~ year,NEI, sum)
png("plot1.png", width = 480, height = 480)
barplot(
        (aggTotals$Emissions)/10^6,
        names.arg=aggTotals$year,
        col = "lightblue", border = TRUE,
        xlab="Year",
        ylab="PM2.5 Emissions (10^6 Tons)",
        main="Total emissions from PM2.5 in the United States from 1999 to 2008 "
)
dev.off()

### Draw plot 2 for total emissions from PM2.5 in the Baltimore City, Maryland

BaltimoreNEI <- subset(NEI, fips == "24510") 
aggTotals2 <- aggregate(Emissions ~ year,BaltimoreNEI, sum)

png("plot2.png", width = 480, height = 480)
barplot(aggTotals2$Emissions,
        names.arg=aggTotals2$year,
        col = "green", border = TRUE,
        xlab="Year",
        ylab="PM2.5 Emissions (Tons)",
        main="total emissions from PM2.5 in the Baltimore City, Maryland from 1999 to 2008 "
)
dev.off()

### Draw plot 3
BaltimoreNEI <- subset(NEI, fips == "24510") 
aggTotals2 <- aggregate(Emissions ~ year,BaltimoreNEI, sum)

library(ggplot2)
png("plot3.png", width = 720, height = 480)
ggp3 <- ggplot(BaltimoreNEI,aes(factor(year),Emissions,fill=type)) +
        geom_bar(stat="identity") +
        theme_bw() + guides(fill=FALSE)+
        facet_grid(.~type,scales = "free",space="free") +
        labs(x="Year", y=expression("Total PM2.5 Emissions (Tons)")) + 
        ggtitle("Total PM2.5 Emissions in Baltimore City 1999-2008 by Source Type")
print(ggp3)
dev.off()

### Draw plot4.png
combustionRelated <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coalRelated <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE)
CC <- (combustionRelated & coalRelated)
combustionSCC <- SCC[CC,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]

aggTotals4 <- aggregate(combustionNEI[c("Emissions")], list(year = combustionNEI$year), sum)

library(ggplot2)
png("plot4.png", width = 720, height = 480)
ggp4 <- ggplot(combustionNEI,aes(factor(year),Emissions/10^5)) +
        geom_bar(stat="identity", fill = "pink", width = 0.75) +
        theme_bw() +  guides(fill=FALSE) +
        labs(x="year", y=expression("Total PM2.5 Emission (10^5 Tons)")) + 
        labs(title=expression("Total PM2.5  Coal Combustion Source Emissions Across US from 1999-2008"))
print(ggp4)
dev.off()

### Draw plot 5

vehicles <- grep("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]
baltimoreVehiclesNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]

library(ggplot2)
png("plot5.png", width = 720, height = 480)
ggp5 <- ggplot(baltimoreVehiclesNEI,aes(factor(year),Emissions)) +
        geom_bar(stat="identity",fill="yellow",width=0.75) +
        theme_bw() +  guides(fill=FALSE) +
        labs(x="year", y=expression("Total PM2.5 Emission (10^5 Tons)")) + 
        labs(title=expression(" Total PM2.5 emissions from motor vehicle sources changed from 1999–2008 in Baltimore City"))
print(ggp5)
dev.off()

### Draw plot 6
# Problem 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
vehiclesBaltimoreNEI <- vehiclesNEI[vehiclesNEI$fips == "24510",]
vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
vehiclesBaltimoreNEI$city <- "Baltimore City"
vehiclesLANEI$city <- "Los Angeles County"
TwoNEI <- rbind(vehiclesBaltimoreNEI,vehiclesLANEI)

library(ggplot2)
png("plot6.png", width = 720, height = 480)
ggp6 <- ggplot(TwoNEI, aes(x=factor(year), y=Emissions, fill=city)) +
        geom_bar(aes(fill=year),stat="identity") +
        facet_grid(.~city, scales="free", space="free") +
        theme_bw() + guides(fill=FALSE)+
        guides(fill=FALSE) + theme_bw() +
        labs(x="year", y=expression("Total PM2.5 Emission (Kilo-Tons)")) + 
        labs(title=expression("Total PM2.5 Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

        
print(ggp6)
dev.off()