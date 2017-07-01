## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <<- NULL
  
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inverse <<- solve
  getSolve <- function() inverse
  
  list(
    set = set,
    get = get,
    setSolve = setSolve,
    getSolve = getSolve
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getSolve()
  if (!is.null(inverse)) {
    message("Getting cache data")
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  x$setSolve(inverse)
  inverse
}
