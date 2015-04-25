
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
    # sets matrix x to given y value and nulls the inverse m
    x <<- y
    m <<- NULL
  }
  
  get <- function() x # returns the matrix x
  
  setinverse <- function(solve) {
    # sets the inverse matrix m
    m <<- solve
  }
  
  getinverse <- function() m 
  # assumes inverse exists
  # returns inverse matrix m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # returns the list of get/set functions
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
  ## assume x is a matrix
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    # if inverse exists then return it and output message
    message("getting cached data")
    return(m)
  }
  # else no inverse. solve x for inverse, then return it
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
