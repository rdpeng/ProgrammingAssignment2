makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse of the matrix to NULL
  inverse <- NULL

  # Create the set function for the matrix
  setMatrix <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }

  # Create the get function for the matrix
  getMatrix <- function() x

  # Create the cache function for the inverse
  cacheInverse <- function(solve) {
    if (!is.null(inverse)) {
      message("Getting cached data.")
      return(inverse)
    }
    inverse <- solve(x)
    inverse
  }

  # Return the list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       cacheInverse = cacheInverse)
}

cacheSolve <- function(x, ...) {
  # Get the cached data
  inverse <- x$cacheInverse(solve)

  # Return the inverse
  inverse
}
