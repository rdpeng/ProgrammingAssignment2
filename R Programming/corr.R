corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    comp.data <- complete(directory)
    k <- subset(comp.data, comp.data$nobs > threshold)
    obj.id <-c(k[,1])
    L<-length(obj.id)
    j<- vector("numeric", length = 0)
    for (i in obj.id) {
        raw.data <- getmonitor(i, directory)
        a <- na.omit(raw.data)
        b <- cor(a$sulfate, a$nitrate)
        j <- append(j, b, after= length(j))
    }
    c(j)
    j
}

?complete