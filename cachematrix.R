## This will create a cached matrix object that can be inversed 
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  ## Get the value of the cached matrix
  setInverse <- function(inverse) cachedInverse <<- inverse
  ## Set the value of the inverse
  getInverse <- function() cachedInverse
  ## Get the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  ## This solves for the inverse of the cached matrix x
  ## Again following the prototype vector example that is provided
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInverse(invFunc)
  invFunc
  }
}
