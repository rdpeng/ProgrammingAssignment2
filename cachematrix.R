
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# instantiate what is essentially a new data type: CacheMatrix.
# a CacheMatrix may be instantiated, the data may be referenced
# using the $set, $get, $setinverse, and $getinverse  methods.
#
# if $set is used then
# the inverse is nulled out and recalculated.
#
# $setinverse is assumed to be used only by cacheSolve().
#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) {
    m <<- solve
  }
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#
# cacheSolve(x) returns the inverse of x
# x is assumed to be of type CacheMatrix.
#
# if inverse exists then output a message "getting cached data."
#
# if inverse does not exist then solve for it and return it.
# no message is output.
#
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    # inverse exists"
    message("getting cached data")
    return(m)
  }
  
  # no inverse. solve x for inverse, then return it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
