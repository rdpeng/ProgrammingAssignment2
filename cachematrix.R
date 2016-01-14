## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  create <- function(y) {
    x <<- matrix(y, nrow = sqrt(length(x)))
    cm <<- NULL
  }
  retrieve <- function() x
  solv <- function(inverse) cm <<- inverse
  cache <- function() cm
  list(create = create, retrieve = retrieve, solv = solv, cache = cache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cm <- x$cache()
  if(!is.null(cm)) {
    message("retrieving cached data")
    return(cm)
  }
  work <- x$retrieve()
  cm <- solve(work)
  x$solv(cm)
  cm
}