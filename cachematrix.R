## Caching the Inverse of an object called Matrix:
## Solving the inverse of a matrix is a computationally intensive process, 
## therefore it is beneficial to obtain the inverse of the matrix without doing many repeated calculations.
## In this order of ideas, two functions are presented here that allow computing 
## the inverse of the matrix without unnecessary computational efforts.

## The first function creates a special "matrix" object that is able to cache its inverse.

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


## The second function obtains the inverse of the matrix created in the first function
## called makeCacheMatrix previously presented. If the program performs the function in the
## correct way we are able to obtain the inverse of matrix directly from the cache.

cacheSolve <- function(x, ...) {
  ## Here, the function return a matrix that is the inverse of "x" object
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
## Finally, at this point, we obtain a specific matrix who is the inverse 
## of original 'x' object

