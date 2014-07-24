## This file contain a functions set for improving the matrix inverse calculation
## by the use of a cache where to store the previously calculed value



## This function creates a cache to store the inverse value of a given matrix
## in a external variable.
## The fuction returns a list containing the methods that are availables to 
## interact with the cache.
## Input variables:
##   x : an invertible matrix (the invertible contidion is not checked before the cache is created)
##
## Use example:
##   > x <- matrix(1:4, 2, 2)
##   > cache <- makeCacheMatrix(x)
##
## Another use example: It considers 'cache' previosly defined with 'makeCacheMatrix' function.
##   > cache$set(matrix(10:13, 2, 2))

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL     ## this is the cached value
    ## method definition
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    ## returned value
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function return either the cached value of the inverse matrix, if it has
## been previously cached, or the calculated value with the "solve()" function (and
## then stores the value in the cache) if not.
##
## Input variables:
##   x : a cached matrix previously created with is created) 'makeCacheMatrix' function
##
## Use example:
##   > x <- matrix(rnorm(16),4,4)
##   > cache <- makeCacheMatrix(x)
##   > cacheSolve(cache)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()   ## get the cached value
    ## if cache isn't empty, return it and exit
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## in other case, calculate the value
    data <- x$get()
    m <- solve(data, ...)
    ## and store it for future calls to this function
    x$setinverse(m)
    ## return the calculated value
    m
    
}
