# The golas of the functions is to add a caching functionality to a matrix
# They will create a matrix that allows this caching functionality and cache the inverse of that matrix
# So when the matrix's inverse is required, it won't always be calculated, but returned from the cache if it was
# previously calculated


# This function accepts a matrix as its parameter and returns a new object that has functions to allow caching

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function does the calculation of the inverse of the matrix, and saves it in "cache" for it to
# be used again if requested

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("get cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  # Returns the inverse of the matrix
  i
}
