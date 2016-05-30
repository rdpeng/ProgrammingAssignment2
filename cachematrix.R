## This pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix. 
## When the inverse has been calculated, the cachesolve should print the inverse from the cache. 
cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   message("compute inverse")
  matData <- x$get()
   inv <- solve(matData, ...)
   x$setinverse(inv)
  inv
}
