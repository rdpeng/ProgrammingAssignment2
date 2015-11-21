## These two functions allow for the user to solve a matrix and cache the resulting value
## If the same matrix is solved for again, the cached value is returned in order to save time

## This function cahces a matrix's inverse
## It also indicates whether a value is currently cached or not
## The list of functions created by this function are used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function returns the inverse of a matrix
## If the inverse has been previously computed the cached result is returned
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}