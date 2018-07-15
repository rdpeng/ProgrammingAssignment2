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


## R-Code For "plot1"

## Create a dataframe that having yearwise values for Emissions for all the counties ("fips") and all the sources ("SCC")    
all_emissn <- aggregate(Emissions~year, ENI, sum)

## print all_emissn and check the magnitude of the values under the Emission   
all_emissn 

## Create a vector "Tot_emiss" and assign to it the values under the all_emissn$Emission/10^6 
Tot_emiss <- all_emissn$Emission/10^6

## Create a vector "Period" and assign to it the unique values under the column "year"   
Period <- levels(as.factor(ENI$year))

## Plot Tot_emiss and Period withouth the axes which we will add later      
plot(Period,Tot_emiss,col = "red", type = "l", xlab = "Years", ylab = "Total Emissions from PM2.5(in million Tons)", xaxt='n', ylim=c(0,10))
points(Period,Tot_emiss,col = "red", pch= 19)

## Add x-axis indicating the years 1999 to 2008 as sequence separated by 3 years  
axis(side=1, at =seq(1999,2008, 3))

## Set the margins and outper margins  
par(mar=c(4,4,2,1), oma = c(0,0,2,0))
box("outer","solid")

## Add summary of the data "Total Emissions from PM2.5 have decreased in US from 1999 to 2008"
mtext("PM2.5 Emissions decreased in US from 1999 to 2008", outer = TRUE)

## Copy the plot to a PNG file "plot1.png" and set the height and width as 600 
dev.copy(png, file = "plot1.png", height= 600, width = 600)

## Close the PNG device
dev.off()
