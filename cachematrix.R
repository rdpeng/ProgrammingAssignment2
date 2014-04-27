## These functions allow you to cache the inverse of a matrix.
## The first function creates a list containing functions to do the following:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of a matrix
## get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invrse <- NULL
  set <- function(y) {
    x <<- y
    invrse <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invrse <<- inverse
  getinv <- function() invrse
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
  
}

## Function first checks if the inverse of a matrix has been computed.
## If computed it returns that cached inverse else it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrse <- x$getinv()
  if(!is.null(invrse)) {
    message("getting cached data")
    return(invrse)
  }
  data <- x$get()
  invrse <- solve(data, ...)
  x$setinv(invrse)
  invrse
  
}
