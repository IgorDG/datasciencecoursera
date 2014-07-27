plot3 <- function ()
{
  ## Reading the data for plotting
  path <- paste(getwd(),"/PA3.2/",sep = "")
  NEI <- readRDS(paste(path,"summarySCC_PM25.rds",sep=""))
  SCC <- readRDS(paste(path,"Source_Classification_Code.rds",sep=""))
  
  ## Extracting the information
  data <- aggregate(NEI[NEI$fips == "24510",]$Emissions,
                    by=list(Year=NEI[NEI$fips == "24510",]$year,
                            Type=NEI[NEI$fips == "24510",]$type),
                    FUN=sum)
  
  ## Creation of the png image with the ggplot2() package
  png(paste(path,"plot3.png",sep=""))  
  
  qplot(Year,x,data = data, facets = . ~ Type, geom="path")
  
  dev.off()  
  
}