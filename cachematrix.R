## This file is a fork of the repository for programming assignment 2.

######################################################################################################
## PS: My submission reuses the code and comment in the given example from the assignment
######################################################################################################

# creates a special "matrix", which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
#
# PS: My submission reuses the code and comment in the given example from the assignment
# 
makeCacheMatrix <- function(x = matrix()) {
  # variable that holds the inverse matrix
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
# it calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setmean function
# 
# PS: This submission reuses the code & comment in the given example from the assignment

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
