## I have created two functions that enable the user to easily and efficiently compute the inverse of a matrix
## by storing the information in a cache.

## makeCacheMatrix is a function that stores a matrix x in the cache and enables the user to get and set the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(m) {
    x <<- m
    n <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) n <<- solve
  getinverse <- function()n
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve is a function that returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data)
  x$setinverse(n)
  n
}
