# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than 
# compute it repeatedly (there are also alternatives to matrix inversion 
# that we will not discuss here).

# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# Write the following functions:
  
# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

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

# cacheSolve: This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# Follwoings are tests for my code
a <- makeCacheMatrix();
summary(a);

a$set( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
a$get();

cacheSolve(a)

# retrieving from the cache in the second run
cacheSolve(a)
