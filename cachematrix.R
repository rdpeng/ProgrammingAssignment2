## This work allows to make the inverse of a matrix 
## using the cache if it happens to
## compute a new inverse of a matrix with the same values. 
## In other words the cache will be used instead
## of calculating the inverse again.

## This function returns a list containing 4 functions
## the 1st allows to set the matrix
## the 2nd returns the matrix itself
## the 3rd set the inverse of the matrix
## the 4th returns the inverse 
## of the matrix if it has been stored previously, otherwise null
makeCacheMatrix <- function(x = matrix()) {
  #temporary variable for solve calculation. Initialization
  s <- NULL
  set <- function(y) {
    x <<- y
    #with every new setting, I have to initialize with null the inner variable
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) s <<- inverse
  getsolve <- function() s
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function returns the inverse of a square matrix, if an inverse with the same
## matrix has been computed before, the cache value will be returned without any calculation

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s))
  {
    message("getting cached inverse")
    return(s)
  }
  s <- solve(x$get(), ...)
  x$setsolve(s)
  s
}
