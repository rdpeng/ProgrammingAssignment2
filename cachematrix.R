Two functions that are used to create a special object that stores a numeric vector and cache's its mean .
  

## Write a short comment describing this function
This function stores (x = matrix()).... in the makecachematrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}



 It first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
} m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
(matrix = x ())
