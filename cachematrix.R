## Put comments here that give an overall description of what your
## functions do

## This function creates a "special matrix to be inverted

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


## This function takes the matrix from "makeCacheMatrix" and inverts it
## then solves for the inverted mean

cacheSolve <- function(x, ...) {
        m <- solve(x$getmean())
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- mean(data,...)
        x$setmean(m)
        m
}
