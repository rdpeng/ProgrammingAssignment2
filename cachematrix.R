## The first function makes a special matrix, the second creates its inverse
## The inverse of the matrix is cached and next time if the inverse command is run it
## first of all checks the cache and then if there is no change in the original matrix 
## then the inverse is fetched from the catche and not recalculated

## This function makes a special matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- solve(x)
  getInverse <- function() inv
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function is to get the inverse of a matrix (x) from the cache if there is no change to x
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}