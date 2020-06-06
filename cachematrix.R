##  Caching the Inverse of a Matrix


## makeCacheMatrix creates a special "matrix" which is a list containing functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {m <<- inverse}
  getInverse <- function() m
  list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solve Inverse of Matrix if not present in cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}
