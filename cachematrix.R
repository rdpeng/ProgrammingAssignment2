## cachematrix computes the inverse of a matrix and stores it in cache so it can retrieved
## later, saving in computing time.

## makeCacheMatrix creates a special "matrix" object than can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' getting its cached value or
        ## computing it if it isn't cached.
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
