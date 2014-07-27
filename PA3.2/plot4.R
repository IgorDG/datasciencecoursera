plot4 <- function ()
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
  
  ## Creation of the png image with the ggplot2() function
  png(paste(path,"plot4.png",sep=""))  
  
  with(data, plot(Year, 
                  x,
                  main = "Total PM2.5 in US (only coal realated emissions)",
                  xlab = "Period of there years",
                  ylab = "Total tons of PM2.5",
                  type = "l"))
  
  dev.off()  
  
}

