## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
invrs <- NULL
set <- function(y) {
x <- y
invrs <- NULL
}
get <- function() x
setinverse <- function(inverse) invrs <- inverse
getinverse <- function() invrs
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      invrs <= x$getinverse()
if(!is.null(invrs)) {
message("getting cached data")
return(invrs)
}
mat <- x$get()
invrs <- solve(mat, ...)
x$setinverse(invrs)
invrs
}
