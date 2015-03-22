## The functions in this file enable the caching of the inverse of a matrix and its retrieval
## in cases where the original matrix is unchanged (instead of recalculating the inverse).

## This function creates a special "matrix" which is really a list containing functions to:
## set the value of the matrix; get the value of the matrix; set the inverse of the matrix;
## and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(updatedMatrix) {
    x <<- updatedMatrix
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If x has a cached inverse, this function returns it;
## otherwise it calculates the inverse, caches it, and returns it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if (!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  i <- solve(x$get())
  x$setInverse(i)
  i
}

