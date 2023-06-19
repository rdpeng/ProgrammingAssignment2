
## makeCacheMatrix function-This function creates a special "matrix" object that can cache its inverse

# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse matrix cache
  inverse <- NULL
  
  # Function to set the matrix
  setMatrix <- function(y) {
    x <<- y  # Use <<- to assign to the parent environment
    inverse <<- NULL  # Reset the inverse cache when matrix changes
  }
  
  # Function to get the matrix
  getMatrix <- function() {
    x
  }
  
  # Function to set the inverse
  setInverse <- function(newInv) {
    inverse <<- newInv
  }
  
  # Function to get the inverse
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions to interact with the matrix object
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Function to compute the inverse of a matrix, caching the result if possible
cacheSolve <- function(cacheMatrix) {
  # Retrieve the matrix
  matrix <- cacheMatrix$getMatrix()
  
  # Check if the inverse is already computed and cached
  inverse <- cacheMatrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # Compute the inverse
  inverse <- solve(matrix)
  
  # Cache the inverse
  cacheMatrix$setInverse(inverse)
  
  # Return the inverse matrix
  inverse
}

