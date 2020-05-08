## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will be requiring a matrix as an input, and the matrix's inverse will be saved in the vector.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL        # Initialize Inverse

# Set matrix in global enviornment
  set = function(y) {
    x <<- y
    inv <<- NULL
  }

  get = function() x     # Function to get the matrix
  setinverse = function(inverse) inv <<- inverse     
  getinverse = function() inv      # Gets inverse value

# List of functions to return
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will first check, whether the matrix is already saved in the Cache or not. If yes, then it will take the output from there
## only, otherwise, it will solve the new matrix which isn't saved in the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()      # gets cached matrix back, if any
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

# If there is no inverse, then get matrix stored in object
  data = x$get()
  inv = solve(data, ...)    # Matrix Multiplication for Inverse

  x$setinverse(inv)         # Set inverse to object
  inv
}
