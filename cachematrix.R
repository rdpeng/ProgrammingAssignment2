## Put comments here that give an overall description of what your
## functions do
setwd('/Users/Reyna/Documents')

## I set the input x as a matrix and then set the new solved value (v) to be null
## Then every reference of mean is changed to solve 
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
}
  get <- function() x
  setsolve <- function(solve) v <<- solve
  getsolve <- function() v
  c(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Just changed mean to solve 
cacheSolve <- function(x, ...) {
  v <- x$getsolve()
  if(!is.null(v)) {
    message("getting inversed matrix")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setsolve(v)
  v
}

