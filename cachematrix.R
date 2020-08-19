#Create inverse of matrix thet can be cahce
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#Retreive data from cache
cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("cached matrix...")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
