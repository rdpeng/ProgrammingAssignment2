## Caching the inverse of a matrix instead of computing it repeatedly will have the
## benefit of reducing the costly computations

## The function makeCacheMatrix() will create a special object that stores the matrix
## and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve() function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above and it will retrieve the inverse from the cache, if
## the inverse has already been calculated (and the matrix has not changed) 



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
