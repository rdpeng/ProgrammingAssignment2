## We will create the basic R-code that will be used to do:
## i)   Download, unzip and store the data file(s) in the pre-set working directory 
## ii)  Read the data into a dataframe
## iii) Check the dataframe,if there are any NA's in the data frame and if there are then eliminiate rows containing the NA's and assign the residual data to another dataframe
## iv)   Get a summary of the data frame so that we can know the class, length and the 5-point summary for all the columns    


## Create a temporary object temp 
temp<-tempfile()

## Downlaod the zipped folder from the url for the zipped folder
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", temp)

## Unzip the "exdata-data-NEI_data.zip" folder to a predefind target directory specifying path "C:/Coursera/Week4Proj" 
unzip(temp, exdir="C:/Coursera/Week4Proj")

## Set working directory to the folder where the zipped folder is stored   
setwd("C:/Coursera/Week4Proj")

## Unlink the temp object 
unlink(temp)

## check the list of file(s) in the directory 
dir()

## read the data file "summarySCC_PM25.rds" to the dataframe "ENI" 
ENI <- readRDS("summarySCC_PM25.rds")

##Read the first few rows, using head() to check the construct of the data file        
head(ENI)

## Check if there are any NA's in the dataframe ENI
any(is.na(ENI))

## check the summary of data under each column
summary(ENI)


## R-Code For "plot3"

## subset ENI to get Baltimore Data (Fips == "24510") 
ENI_Balti <- subset(ENI, ENI$fips == "24510")

## Create a dataframe having type-wise sum of all Emissions for Baltimore from all the sources ("SCC")    
all_emissn <- aggregate(Emissions~ type+year, ENI_Balti, sum)

## print all_emissn and check the magnitude of the values under the Emission   
all_emissn 


## Load the ggplot2 library
library(ggplot2)
g <- ggplot(all_emissn, aes(year,Emissions))

g + geom_line(aes(color= type),lty =1, lwd = 1)+labs(title = "Baltimore City PM2.5 Emisison Levels by Types of Source", x="Years",y = "Total Emission from PM2.5(in Tons)")+scale_x_continuous(breaks= c(1999,2002,2005,2008)) + theme(plot.title = element_text(hjust = 0.5))


## Copy the plot to a PNG file "plot3.png" and set the height and width as 600 
dev.copy(png, file = "plot3.png", height= 600, width = 600)

## Close the PNG device
dev.off()