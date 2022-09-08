##Don't fix what's not broken. Used the example as a set up
##And edited where needed.

## This script will cache the inverse of a matrix,
##The first set of function will create a list of functions to set a matrix
##and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  nvrt <- NULL
  set <- function(y) {
    x <<- y
    nvrt <<- NULL
  }
  get <- function() x
  setinv <- function(inv) nvrt <<- inv
  getinv <- function() nvrt
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second set of function solves for a matrix's inverse
## Will compute the matrix inverse
## if it is not there already in cache of passed object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  nvrt <- x$getinv()
  if(!is.null(nvrt)) {
    message("getting cached data")
    return(nvrt)
  }
  data <- x$get()
  nvrt <- solve(data, ...)
  x$setinv(nvrt)
  nvrt
}