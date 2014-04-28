## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the Matrix
##set the value of the inverse
##get the value of the inverse

makeMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## this function checks to see if the mean has already been calculated
## yes - gets the mean from the cache and skips the computation. 
## no - it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheinverse <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
