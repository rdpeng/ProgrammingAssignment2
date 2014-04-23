
## This function is use to cache mean, initially if the mean is Zero it computes the mean, if not it computes the mean.
## and return the mean.


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
