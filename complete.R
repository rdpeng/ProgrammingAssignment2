#Write a function that reads a directory full of files and reports 
#the number of completely observed cases in each data file. The function 
#should return a data frame where the first column is the name of the 
#file and the second column is the number of complete cases. 

complete <- function(directory, id = 1:332) {
        old_path <-setwd(getwd())
        path <- setwd(paste0(getwd(),"/", directory))
        files = list.files(pattern = "*.csv") #list of all files in directory
        result<-data.frame()
        
        for(i in 1:length(id)){
                data <- read.csv(files[id[i]])
                nobs<-sum(as.numeric(complete.cases(data)))
                data_frame <- data.frame("id" = id[i], "nobs" = nobs)
                result<-rbind(result, data_frame)
        }
        setwd(old_path)
        return(result)
}