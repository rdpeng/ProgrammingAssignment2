#Write a function that takes a directory of data files and a threshold 
#for complete cases and calculates the correlation between sulfate and 
#nitrate for monitor locations where the number of completely observed 
#cases (on all variables) is greater than the threshold. The function 
#should return a vector of correlations for the monitors that meet the 
#threshold requirement. If no monitors meet the threshold requirement, 
#then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
        old_path <-setwd(getwd())
        vec <- numeric(0)
        #create table with all complete cases with function complete() 
        #from source(complete.R)
        table_all_cc <- complete(directory)
        
        path <- setwd(paste0(getwd(),"/", directory))
        files = list.files(pattern = "*.csv")
        #table with monitors with complete cases which are greater 
        #than the threshold
        table_cc <- table_all_cc[table_all_cc$nobs >= threshold,]
        
        for(i in table_cc$id){
                one_table <- read.csv(files[i])
                data <- one_table[complete.cases(one_table),]
                vec <- c(vec, cor(data$sulfate, n<-data$nitrate))
        }
        setwd(old_path)
        return(vec)
}
