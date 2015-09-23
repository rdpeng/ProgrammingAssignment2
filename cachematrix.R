## Caching the Inverse of a Matrix
## The goal is to create a pair of functions that cache the inverse
## of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    ## the special "matrix" object has these 4 functions: 
    ## set, get, setinverse, getinverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.
## Computing the inverse of a square matrix can be done with the 
## solve function in R. (ex. if x is a square invertible matrix, then 
## solve(x) returns its inverse.)

cacheSolve <- function(x, ...) {
    ## x$getinverse uses the getinverse function of the special object x
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Note the following example for usage:
##
## > x <- matrix(c(3, -7, 5, 2), 2, 2)
## > x
## [,1] [,2]
## [1,]    3    5
## [2,]   -7    2
## > cacheSolve(makeCacheMatrix(x))
## [,1]        [,2]
## [1,] 0.04878049 -0.12195122
## [2,] 0.17073171  0.07317073
## > cacheSolve(makeCacheMatrix(cacheSolve(makeCacheMatrix(x))))
## [,1] [,2]
## [1,]    3    5
## [2,]   -7    2
