## Put comments here that give an overall description of what your
## functions do.  
Functions tell the program what actions to take depending on the code that is entered.
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


## Write a short comment describing this function
This function solves for x.
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
