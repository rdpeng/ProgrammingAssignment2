# In the readme file I did the runcode and the test results.

# The first function, makecacheMatrix creates a special "matrix", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
# the new function is structured according the MakeVector function in the 
# assignment example

makeCacheMatrix <- function(Input_Matrix = numeric()) {
  
  # holds the cached inversed matrix or NULL if nothing is cached yet
  # initially nothing is cached that is why the variable Inverse_Matrix it is set to NULL
  Inverse_Matrix <- NULL
  
  # store a matrix
  
  setMatrix <- function(newMatrix) {
    Input_Matrix <<- newMatrix
    # since the matrix is assigned a new matrix, drop the Inverse_Matrix
    Inverse_Matrix <<- NULL
  }
  
  
  # returns the input matrix
  getMatrix <- function() {
    Input_Matrix
  }
  
  
  # cache inverse matrix 
  cacheInverse <- function(solve) {
    Inverse_Matrix <<- solve
  }
  
  
  # get the cached inverse matrix
  getInverse <- function() {
    Inverse_Matrix
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}




# The following cacheSolve function gets the inverse out of cache or calculates the inverse if not available

cacheSolve <- function(Inverse_Matrix, ...) {
  # get the cached matrix
  inverse <- Inverse_Matrix$getInverse()
  # if a cached matrix exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- Inverse_Matrix$getMatrix()
  inverse <- solve(data)
  Inverse_Matrix$cacheInverse(inverse)
  
  # return the calculated matrix
  inverse
}

