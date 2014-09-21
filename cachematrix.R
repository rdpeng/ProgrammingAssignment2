## Put comments here that give an overall description of what your
## functions do

## Function to create a cache matrix. Takes an optional matrix argument.
## Inside it has four functions defined. 2 setters and 2 getters to set and get the data matrix and inverse of the data matrix

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


## Function to get the inverse for a given cacheMatrix
## Computes inverse if inverse is not in cache, else returns the value from cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
