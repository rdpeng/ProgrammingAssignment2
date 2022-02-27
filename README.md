### Assignment: Caching the Inverse of a Matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
      message("Getting the inverse of the matrix")
      return(i)
  }
  imat <- x$get()
  i <- solve(imat, ...)
  x$setsolve(i)
  i
}
