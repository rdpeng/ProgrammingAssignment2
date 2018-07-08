## Assignment3: Caching the Inverse of a Matrix
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    # Set the value
    m <<- NULL # Clear the cache
    
  }
  
  get <- function() x
  # Define function to set the inverse. This is only used by getinverse() when
  # no cached inverse
  setInverse <- function(inverse) m <<- inverse
  # Define function to get inverse
  getInverse <- function() m
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  m <- x$getInverse() # This fetches the cached value for inverse
  if(!is.null(m)) { # If the cache was not empty, it just return it
    message("getting cached data")
    return(m)
  }
  # The cache was empty. it is need to calculate it, cache it, and then return it.
  data <- x$get()  # Get value of matrix
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  m                # Return the inverse
}
