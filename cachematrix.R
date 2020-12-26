## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
# These pair of functions cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
k <- NULL
set <- function(y){
x <<- y
k <- NULL
}
get <- function()x
setInverse <- function(inverse) j <<- inverse
getInverse <- function() k
list(set = set, get = get, 
setInverse = setInverse,
getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse
# has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
k <- x$getInverse()
if(!is.null(k)){
  message("getting cached data")
  return(k)
}
mat <- x$get()
k <- solve(mat,...)
x$setInverse(k)
k
}
