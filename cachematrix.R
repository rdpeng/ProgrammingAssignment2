## make X as as input matrix
## set the solved value "sol" as a null
## change every reference to "mean" to "solve"

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) sol <<- solve
  getsolve <- function() sol
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
## cacheSolve is a function which computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
  sol <- x$getsolve()
  if(!is.null(sol)) {
    message("getting inversed matrix")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}

