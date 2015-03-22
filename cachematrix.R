## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the
# matrix. Contains the following functions:
# setMatrix set the value of a matrix
# getMatrix get the value of a matrix
# cacheInverse get the cahced value (inverse of the matrix)
# getInverse get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  m <- NULL
  # store a matrix
  set <- function(y) {
    x <<- y
    # since the matrix is assigned a new value, flush the cache
    m <<- NULL
  }
  # returns the stored matrix
  get <- function() x
  # cache/set the given argument  
  setinv <- function(inv) m <<- inv
  # get the cached value
  getinv <- function() m
  # return a list. Each named element of the list is a function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# The following function calculates the inverse of a "special" matrix created with
# makeCacheMatrix
cacheSolve <- function(x, ...) {
  # get the cached value
  m <- x$getinv()
  # if a cached value exists return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$get()
  m <- solve(data)
  x$setinv
  # return the inverse
  m
}
