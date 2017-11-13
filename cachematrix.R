

## Write a short comment describing this function

# Function: makeCacheMatrix
# Takes a matrix object and builds an object intended to cache the
# inverse of itself.  Intended for use with function cacheSolve()
#
# returns a list of the setter and getter functions

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix
  inv_x <- NULL
  
  # setter function
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  # getter function
  get <- function() x
  
  # setter for inverse matrix
  setinv <- function(inv) inv_x <<- inv
  
  # getter for inverse matrix
  getinv <- function() inv_x
  
  # return a list of the functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function cacheSolve 
# Returns the inverse of matrix x assumed to be a square invertable
# matrix.  The method is efficient in that the inverse is cached and
# subsequent calls will return the cached inverse of the matrix.

cacheSolve <- function(x, ...) {
  # attempt to retrieve possibly cached matrix
  inv_x <- x$getinv()
  
  # If cached object found, report & return.
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  
  # If not cached, get the original matrix & solve for it's inverse
  data <- x$get()
  inv_x <- solve(data,...)
  
  # Cache the inverse matrix
  x$setinv(inv_x)
  
  ## Return a matrix that is the inverse of 'x'
  inv_x
}
