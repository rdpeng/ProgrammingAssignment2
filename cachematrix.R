## Put comments here that give an overall description of what your
## functions do

## These two functions are intended for creating a special list 
##  containing the original matrix as well as its inverse and cach it so that
##  we can return it faster in the future.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    returrn(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
  }
