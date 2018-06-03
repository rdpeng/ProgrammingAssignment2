## This R program contains 2 functions to calcualate the inverse of a matrix,
## store it in the cache. if available in cache, use rather than compute

## the function makeCacheMatrix stores the following in the list
# set the value of matrix
# get the value of matrix
# set the value of inverse 
# get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the function cacheSolve() checks if the cache contains the inverse
# if not found, it calculates the inverse and sets into the cache for future use


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
