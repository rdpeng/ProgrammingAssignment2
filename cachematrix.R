##Write the following functions:
##◦makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {#creating a function
  inv <- NULL #defining inverse  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #setting and getting inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computation of inverse of the "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated then inverse from the cache will be retrieved.

cacheSolve <- function(x, ...) {
  ##  inverse of 'x' matrix is returned
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



