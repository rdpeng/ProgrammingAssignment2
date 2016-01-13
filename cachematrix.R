## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(nrow = sqrt(length(x)))) {
  cm <- NULL
  create <- function(y) {
    x <<- y
    cm <<- NULL
  }
  retrieve <- function() x
  solv <- function(solve) cm <<- solve
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
  cd <- solve(work)
  x$sol(cm)
  cm
}