## Put comments here that give an overall description of what your
## functions do
## My functions, first, create a matrix that can cache its inverse and calculate the inverse of the matrix returned
## by the first function. If the inverse has been already calculated, the second function retrieve it from the cache.

## Write a short comment describing this function
## This "makeCacheMatrix" function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This "cacheSolve" function calculates the inverse of the matrix returned by "makeCacheMatrix". If the inverse has 
## already been calculated, this function retrieves it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
