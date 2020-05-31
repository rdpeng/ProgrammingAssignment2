
#-----------------------------------------------------------------------------------------------------------------

## The makeCacheMatrix function creates a 'matrix' object  that can cache its inverse. 
## This function expects matrix as an input parameter and then creates and returns a list containing a function to
# 1. set the value of the matrix 
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

#------------------------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inv){ inverse <<- inv}
  getInverse <- function() {inverse}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#-----------------------------------------------------------------------------------------------------

## This function returns the inverse of the matrix created by
## makeCacheMatrix function. It first checks if the inverse has already been calculated.
## If it is, the function skips the computation and retrieves the inverse from the cache. 
## If not, it computes the inverse, sets the value in the cache via setInverse function.

#--------------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

