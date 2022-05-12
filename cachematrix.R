## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  
  # set function sets a new matrix and sets 'inv' to NULL so that the cachesolve 
  # will re-calculate the inverse for the new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function returns the matrix
  get <- function() x
  
  # setinv function sets inv = inverse of matrix (called to by cacheSolve 
  # after calculating inverse in order to save the inverse matrix)
  setinv <- function(inverse) inv <<- inverse
  
  #getinv function returns the saved inverse matrix set by cacheSolve
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get inverse matrix value from makeCacheMatrix function
  inv <- x$getinv()
  # inv is null if inverse matrix has previously been stored
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if inverse matrix has not previously been stored (as with new set matrix)
  # then get matrix, calculate inverse and store inverse matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  #return inverse matrix
  inv
}