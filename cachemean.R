cachemean <- function(x, ...) {
        m <- x$getmean() # first checks to see if the mean has already been calculated 
        if(!is.null(m)) { # if so gets the mean from the cache and skips the computation
                message("getting cached data")
                return(m)
        }
        data <- x$get() # otherwise calculates mean 
        m <- mean(data, ...)
        x$setmean(m)
        m
}