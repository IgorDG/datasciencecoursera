plot6 <- function ()
{
  ## Reading the data for plotting
  path <- paste(getwd(),"/PA3.2/",sep = "")
  NEI <- readRDS(paste(path,"summarySCC_PM25.rds",sep=""))
  SCC <- readRDS(paste(path,"Source_Classification_Code.rds",sep=""))
  
  ## Extracting the information
  coalSCC <- unique(SCC[grepl(SCC$EI.Sector,pattern="Coal"),]$SCC)
  data <- NEI[NEI$SCC %in% coalSCC,]
  data <- aggregate(data$Emissions,
                    by=list(Year=data$year),
                    FUN=sum)
  ## Data from vehicles
  vehSCC <- unique(SCC[grepl(SCC$EI.Sector,pattern="Vehicle"),]$SCC)
  data <- NEI[NEI$SCC %in% vehSCC,]
  ## Data from countys
  data <- data[data$fips == "24510" | data$fips == "06037",]
  ## Aggregating data
  data <- aggregate(data$Emissions,
                    by=list(Year=data$year,County=data$fips),
                    FUN=sum)
  
  ## Creation of the png image with the ggplot2() function
  png(paste(path,"plot6.png",sep=""))  
  
  with(data, qplot(x=Year, 
                  y=x,
                  data = data,
                  geom="path",
                  color=County,
                  main = "Total PM2.5 in Baltimore City vs. Los Angeles County (only vehicle realated emissions)",
                  xlab = "Period of there years",
                  ylab = "Total tons of PM2.5"))
  
  ## qplot(Year,x,data = data, facets = . ~ County, geom="path")
  
  dev.off()  
  
}
