## A pair of functions that cache the inverse of a matrix.
#
# Creates a matrix that can cache it's inverse
# Args:
#   x: A matrix (Optional)
#
# Returns:
#   A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
# catched inverse of a matrix
  j <- NULL 
#getter/setter for matrix
  set <- function(y){
  x <<- y
  j <<- NULL
  }
#getter/setter for matrix inverse
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
# return list of functions for matrix
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}
## Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
#
# Args:
#   x: A matrix
#   ...: Extra arguments
#
# Returns:
#   The inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  
  # return cached matrix inverse if it's been already computed
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  # compute inverse of matrix
  mat <- x$get()
  j <- solve(mat,...)
  # cache inverse
  x$setInverse(j)
  # return inverse of matrix
  return(j)
}
}
