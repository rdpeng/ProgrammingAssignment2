## Cache the inverse of a matrix so you don't have to constantly recompute

## makeCacheMatrix creates a list that will get all the components needed for the 
## next function (setting matrix values, getting matrix values, setting matrix
## inverse, getting matrix inverse)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y = matrix()){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inverse <- solve
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of cached matrix

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinv(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
