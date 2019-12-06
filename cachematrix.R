## The following functions are going to creating a matrix for the outcome of the 
## inverse to be cached
## makeCacheMatrix will save the cache inverse in a matrix format
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {
    x <<- y 
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function( ) i
  list(set = get,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Once the matrix is saved, cacheSolve will solve for the inverse of the cached data
## the result will return the inverse in a matrix  
cacheSolve <- function(x, ...) {
  i <- x$getinverse ( )
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get ( )
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
