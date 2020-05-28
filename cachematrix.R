## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## define the argument with default mode of "matrix"
  
  inv <- NULL ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {
    ## define the set function to assign new 
    x <<- y ## value of matrix in parent environment

    inv <<- NULL ## if there is a new matrix, reset inv to NULL

  }
  get <- function() x  ## define the get fucntion - returns value of the matrix argument

  setInverse <- function(inverse) inv <<- inverse
  ## assigns value of inv in parent environment
  
  getInverse <- function() inv
  ## gets the value of inv where called
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  ## you need this in order to refer 
  ## to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

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
