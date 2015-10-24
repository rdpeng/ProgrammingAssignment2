# R programming - Data Science Specialization
# Assignment 2
# Alicia Vázquez


## makeCacheMatrix creates a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse matrix
  inv <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Returns original matix
  get <- function() x
  # set the inverse matrix
  setinverse <- function(solve) inv <<- solve
  # returns inverse matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes and caches inverse and returns it, if already computed it only retrieves it and returns it.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # Gets the inverse matrix
  inv <- x$getinverse()
  # If inverse matrix is calculated,is retrieved
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # If not calculated, it is calculated
  data <- x$get()
  inv <- solve(data, ...)
  # caches inverse matrix
  x$setinverse(inv)
  inv
}
