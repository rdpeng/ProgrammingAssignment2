# This file contains two functions.
# "makeCacheMatrix" is a function which lists other functions to store
# the cached values
# "cacheSolve" is a function which uses the cached values on
# makeCacheMatrix object and obtain the inverse of these values.


# This function is a list of another functions to help
# storing the cached values
makeCacheMatrix <- function(x = matrix()) {
  
  # Initiate cache value as empty (NULL)
  cacheValue <- NULL
  
  # Set the value for caching
  setCache <- function(value) {
    cacheX <<- value
    cacheValue <- NULL
  }
  
  # Get the stored value which is going to be cached
  getCache <- function() {
    cacheX
  }
  
  # Set the inverse value as cached value
  setInverse <- function(inverse) {
    cacheValue <<- inverse
  }
  
  # Get the cached value
  getInverse <- function() {
    cacheValue
  }
  
  # Return the list of functions
  list(
    setCache = setCache,
    getCache = getCache,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

# Calculate the inverse of a value "x"
cacheSolve <- function(x, ...) {
  
  # Get the last cached value on "x"
  inverse <- x$getInverse()
  
  # Compare if cache value is not empty
  if(!is.null(inverse)) {
    message("getting cached data")
    # If cache value exists, return that value
    return(inverse)
  }
  
  # If not, get the stored value on "x"
  value <- y$getCache()
  # Calculate the inverse of stored value
  inverse <- solve(value, ...)
  # Set the new inverse value as chaced value on "x"
  y$setInverse(inverse)
  
  # Finally, return the inverse value
  inverse
}
