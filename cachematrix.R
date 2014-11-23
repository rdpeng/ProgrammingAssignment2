## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(_m) {
    x <<- _m
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(_inv) inv <<- _inv
  getinv <- function() inv
  list(set=set, get=get, setmean=setmean, getmean=getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

