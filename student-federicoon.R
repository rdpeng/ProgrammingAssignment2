## The following functions implement a special "matrix" object capable to cache the value 
## of its inverse so that it can be looked up in the cache rather than recomputed
## Student federicoon 
## source("https://github.com/federicoon/ProgrammingAssignment2/blob/master/cachematrix.R")

## Initializes a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Computes the inverse of the special "matrix". If it has already been calculated
## (and the matrix has not changed), then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}