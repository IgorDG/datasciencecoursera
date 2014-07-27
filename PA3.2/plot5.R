plot5 <- function ()
{
  ## Reading the data for plotting
  path <- paste(getwd(),"/PA3.2/",sep = "")
  NEI <- readRDS(paste(path,"summarySCC_PM25.rds",sep=""))
  SCC <- readRDS(paste(path,"Source_Classification_Code.rds",sep=""))
  
  ## Extracting the information
  vehSCC <- unique(SCC[grepl(SCC$EI.Sector,pattern="Vehicle"),]$SCC)
  data <- NEI[NEI$SCC %in% vehSCC,]
  data <- aggregate(data[data$fips == "24510",]$Emissions,
                    by=list(Year=data[data$fips == "24510",]$year),
                    FUN=sum)
  
  ## Creation of the png image with the ggplot2() function
  png(paste(path,"plot5.png",sep=""))  
  
  with(data, plot(Year, 
                  x,
                  main = "Total PM2.5 in Baltimore City (only vehicle \
                  realated emissions)",
                  xlab = "Period of there years",
                  ylab = "Total tons of PM2.5",
                  type = "l"))
  
  dev.off()  
  
}
