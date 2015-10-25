## These functions allow you to create a CacheMatrix object which
## can cache its inverse. To use, create a matrix 'x', and pass 'x' to
## makeCacheMatrix(). The returned object 'cx' will be a CacheMatrix.
## Calling cacheSolve('cx') will try to find the inverse matrix from
## the cache. If the inverse is not yet cached, it will be solved and
## cached for future use.

# Creates a CacheMatrix object. The object returned will have several
# functions that allow the inverted matrix to be cached.
makeCacheMatrix <- function(x = matrix()) {
  # Declare the inverse variable. Has not been calculated yet.
  inverse <- NULL
  
  # If this function is called, we have received a new matrix.
  # The inverse is set to null to invalidate old cache data (if any).
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Return the original matrix.
  get <- function() x
  
  # Cache the inverse matrix computed elsewhere.
  setInverse <- function(newInverse) inverse <<- newInverse
  
  # Return the inverted matrix stored in cache (if any).
  getInverse <- function() inverse
  
  # Create a list of methods so they can be accessed by name.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Attempts to get the inverse from the cache. Otherwise it will calculate the inverse,
# and cache the inverted matrix for later use. Either way, it will return the inverted matrix.
cacheSolve <- function(x, ...) { 
  
  # Try to get inverse from cache.
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("Obtained inverse from cache.")
  }
  else{
    # Since the result was null, the inverse has not yet been computed.
    # Obtain the original matrix and solve its inverse.
    originalMatrix <- x$get()
    inverse <- solve(originalMatrix)
    
    #Cache the inverse matrix.
    x$setInverse(inverse)
  }
  
  inverse
}
