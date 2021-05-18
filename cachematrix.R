

makeCacheMatrix <- function(x = matrix()) {
        y <- NULL
  set <- function(z) {
    x <<- z
    y <<- NULL
}
get <- function() x
  setsolve <- function(solve) y <<- solve
  getsolve <- function() y
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
        }


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         y <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(y)
  }
  data <- x$get()
  y <- solve(data, ...)
  x$setsolve(s)
  y
}
