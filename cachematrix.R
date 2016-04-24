## Author: Sandra Guerrero
## functions do:  Return the inverse of matrix with determinant that is not zero, and save the result in cache

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
inverse <- NULL
  set <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix,
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setInverse(inverse)
  inverse
}

# Example
# matriz <- matrix(c(3:10, 10), 3, 3)
# cacheSolve(makeCacheMatrix(matriz))
# output
#           [,1] [,2] [,3]
# [1,] -3.333333    4   -1
# [2,]  3.333333   -5    2
# [3,] -1.000000    2   -1
