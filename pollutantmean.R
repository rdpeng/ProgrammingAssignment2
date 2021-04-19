#Write a function named 'pollutantmean' that calculates 
#the mean of a pollutant (sulfate or nitrate) across a 
#specified list of monitors. The function 'pollutantmean' 
#takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that 
#monitors' particulate matter data from the directory specified 
#in the 'directory' argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <-function(directory, pollutant, id = 1:332){
        old_path <-setwd(getwd())
        path <- setwd(paste0(getwd(),"/", directory))
        files = list.files(pattern = "*.csv")
        result<-NULL
        
        if(length(id)==1) {
                data <- read.csv(files[id]) #for one monitor
        } else {
                data<- do.call(`rbind`,lapply(files[id], read.csv, header=T)) # n monitors in one table
        }
        if(pollutant == "sulfate") {
                sul_na <- is.na(data$sulfate)
                result<-mean(data$sulfate[!sul_na])
        } else if(pollutant == "nitrate") {
                nit_na <- is.na(data$nitrate)
                result<-mean(data$nitrate[!nit_na])
        }
        setwd(old_path)
        return(result)
}