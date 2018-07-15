## We will create the basic R-code that will be used to do:
## i)   Download, unzip and store the data files in the pre-set working directory 
## ii)  Read the data files into different dataframes ( ENI for the "PM2.5 Summary" and SCC for the "Source Classification")
## iii) Check the ENI dataframe if there are any NA's in the data frame and if there are then eliminiate rows containing the NA's and assign the residual ## data to another dataframe
## iv)   Get a summary of the dataframes ENI as well as SCC to know the class, length and the 5-point summary for all the columns 
## From the summary of the SCC dataframe and the EPA documentation(https://www.epa.gov/sites/production/files/2016-12/documents/nei2014v1_tsd.pdf) identify which columns in the SCC dataframe should be used for subsetting basis "Coal Combustion" and "Motor Vehicles"  
## Use strings "Coal" or "Vehicles" for partial matching and creating the SCC subsets as per the requirement   
## Accordingly, basis the 8-digit SCC codes available in the SCC subset, subset of the ENI dataframe  


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

## read the data file "Source_Classification_Code.rds" to the dataframe "SCC" 
SCC <- readRDS("Source_Classification_Code.rds")
##Read the first few rows, using head() to check the construct of the data file        
head(SCC)

## For emission from Motor Vehicle sources, subset the SCC dataframe matching "Vehicles" string in the column EI.Sector      
SCC_Vehicles <- subset(SCC, grepl("Vehicles", EI.Sector))

## Now subset the ENI dataframe so that it has only those SCC-codes which are within the SCC_Vehicles dataframe  
ENI_Vehicles <- subset(ENI, SCC %in% SCC_Vehicles$SCC)

## R-Code For "plot5"

## subset ENI_Vehicles to get Baltimore Data (Fips == "24510") 
ENI_Vehicles_Bal <- subset(ENI_Vehicles, ENI_Vehicles$fips == "24510")

## Create a dataframe having yearwise total values for Emissions     
all_emissn <- aggregate(Emissions~year, ENI_Vehicles_Bal, sum)

## Print all_emissn and check the magnitude of the values under the Emission   
all_emissn 

## Create a vector "Tot_emiss" and assign to it the values under the all_emissn$Emission 
Tot_emiss <- all_emissn$Emission

## Create a vector "Period" and assign to it the unique values under the column "year"   
Period <- as.numeric(levels(as.factor(ENI_Vehicles_Bal$year)))

## Plot Tot_emiss and Period withouth the axes which we will add later      
plot(Period,Tot_emiss,col = "red", type = "l", xlab = "Years", ylab = "Motor Vehicles PM2.5 Emissions (in thousand Tons)", xaxt='n', ylim=c(0,500))
points(Period,Tot_emiss,col = "red", pch= 19)


## Add x-axis indicating the years 1999 to 2008 as sequence separated by 3 years  
axis(side=1, at =seq(1999,2008, 3))


## Set the margins and outper margins  
par(mar=c(4,4,2,1), oma = c(0,0,2,0))
box("outer","solid")

## Add summary of the data "Motor Vehicles PM2.5 Emissions have decreased in Baltimore City from 1999 to 2008"
mtext("Motor Vehicles PM2.5 Emissions have decreased in Baltimore City from 1999 to 2008", outer = TRUE)


## Copy the plot to a PNG file "plot5.png" and set the height and width as 600 
dev.copy(png, file = "plot5.png", height= 600, width = 600)

## Close the PNG device
dev.off()
