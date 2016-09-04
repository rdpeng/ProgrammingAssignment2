## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # var for inverse matrix is initializing.
    inv <- NULL
    
    # setter
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # getter
    get <- function() x
    
    # set the inverse data
    setInverse <- function(inverse) inv <<- inverse
    
    # get the inverse data
    getInverse <- function() inv
    
    # return a list.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
    # get the cache data
    inv <- x$getInverse()
    
    # if cache data is not null, return cache data
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # get original data
    data <- x$get()
    
    # inverse the original data
    inv <-  solve(data, ...)
    
    # set the inverse data to x
    x$setInverse(inv)
    
    # return the inverse data
    inv
}
