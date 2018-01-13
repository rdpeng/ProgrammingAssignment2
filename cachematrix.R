## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}

# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
#
# Args:
#   x: A matrix
#   ...: Extra arguments
#
# Returns:
#   The inverse of the matrix


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}

