makeCacheMatrix <- function(matrix = numeric()) {
  inverse <- NULL
  set <- function(m) {
    matrix <<- m
    inverse <<- NULL
  }
  get <- function() matrix
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cachesolve <- function(matrix, ...) {
  inv <- matrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- matrix$get()
  inv <- solve(m, ...)
  matrix$setinverse(inv)
  inv
}