## Put comments here that give an overall description of what your
## functions do

## Creating a list of functions to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL  
  }
  get <- function() x
  setinv <- function(solve) c <<- solve
  getinv <- function() c
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }


## Computing new or retrieving existing inverse of matrix computed
## with makeCacheMatrix().
cachesolve <- function(x, ...) { 
    c<- x$getinv()
    if(!is.null(c)) {
      message("Getting Cached Data")
      return(c)
    }
    data <- x$get()
      c<- solve(data, ...)
      x$setinv(c)
      c
}
