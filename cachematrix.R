# This file contains two functions which, when combined, creates the inverse of a user's matrix and stores it to reduce further operation costs.
# The first, makeCacheMatrix, stores a user's matrix for later inversing operations.

makeCacheMatrix <- function(x = matrix()) {
  # x = user's matrix
  # Creat a list containing functions to:
  #              1. store the original matrix
  #              2. get the original matrix
  #              3. store the inverse matrix
  #              4. get the inverse matrix
  # This list will be used as the input to cacheSolve().
  
  inv = NULL
  set = function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  
  # Return the list with the stored matrix.
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# The second, cacheSolve, computes the inverse of the user's matrix and returns it.

cacheSolve <- function(x, ...) {
  # x = the output from makeCacheMatrix
  
  # Store the current value of the inverse to inv.
  inv <- x$getinv()
  
  # Check if the value stored is actually the inverse and if it is,   return it.
  if (!is.null(inv))
  {
    message("Getting cached data.")
    return(inv)
  }
  
  # If the value stored is not the inverse, then calculate the inverse. 
  matrx = x$get()
  inv = solve(matrx, ...)
  
  # Stores the value of the inverse in the cache using the setinv function.
  x$setinv(inv)
  
  # Return the inverse of the matrix.
  inv
}
