## The tow functions below cache the inverse of a matrix instead of comupting it repeatedly
## The point to highlight is that this function shows how to use closures. Both inv and x are defined in the below function will only be stored inside the enclosing environment. 

## This function creates a special ???matrix??? object that can cache its inverse

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

## This function computes the inverse of the special ???matrix??? returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the 
## cache. Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a 
##square invertible matrix, then solve(X) returns its inverse.

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
}
