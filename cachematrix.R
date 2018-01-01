## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly 

## Function creates special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" 
## created by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then it should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}