## The two functions, makeCacheMatrix and cacheSolve will store a matrix, calculate
## the inverse of the matrix and store the inverse in cache.  

## makeCacheMatrix creates an R object that stores a matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachesolve() requires an argument that is returned by makeCacheMatrix() 
## in order to retrieve the inverse matrix from the cached value that is 
## stored in the makeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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