# The function makeCacheMatrix() returns a matrix 
# as parameter to the cacheSolve() function.

# This function creates a matrix object which is in fact
# a list containing 4 functions, to get and set the value of the 
# matrix passed to the function, and to get and set the inverse of
# that matrix.

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function computes the inverse of the matrix that
# is passed to it as an argument created by the makeCacheMatrix function.
# The function will either cache the inverse or calculate the inverse
# and return the calcaulted matrix.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
