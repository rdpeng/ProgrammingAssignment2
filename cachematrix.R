## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## CacheSolve function computes the inverse of the matrix created by makeCacheMatrix above. If the inverse has already been calculated and the matrix has not changed, then we get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat,...)
    x$setInverse(i)
    i
}
