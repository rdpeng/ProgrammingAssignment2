## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newx) {
    x <<- newx
    inverse <<- NULL
  }
  setinverse = function(newinv) {
    inverse <<- newinv
  }
  get <- function() x
  getinverse <- function() inverse
  list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  if (!is.null(inv)) {
    message("getting cached value")
    return (inv)
  }
  inv = solve(x$get(), ...)
  x$setinverse(inv) 
  inv
}
