plot1 <- function ()
{
  ## Reading the data for plotting
  path <- paste(getwd(),"/PA3.2/",sep = "")
  NEI <- readRDS(paste(path,"summarySCC_PM25.rds",sep=""))
  SCC <- readRDS(paste(path,"Source_Classification_Code.rds",sep=""))
  
  ## Extracting the information
  data <- aggregate(NEI$Emissions,
                    by=list(Year=NEI$year),
                    FUN=sum)
  
  ## Creation of the png image with the plot() function
  png(paste(path,"plot1.png",sep=""))  
  with(data, plot(Year, 
                  x,
                  main = "Total PM2.5 in US",
                  xlab = "Period of there years",
                  ylab = "Total tons of PM2.5",
                  type = "l"))
  dev.off()  
  
}