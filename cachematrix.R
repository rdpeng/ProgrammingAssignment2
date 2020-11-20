## Put comments here that give an overall description of what your
## functions do

## Creates the function that caches the matrix that is created

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)

}


## Returns the cached matrix or solves the matrix if not already cached and then
## caches it

cacheSolve <- function(x, ...) {inv <- x$getinverse()
if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinverse(inv)
inv

}
