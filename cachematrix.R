## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## The cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix, if the inverse has already been calculated 
## then cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, 
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve function returns the retrieved inverse of a matrix from makeCacheMatrix function
## or calculates the inverse of a matrix.


cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$set_inverse(inv)
  inv
}
