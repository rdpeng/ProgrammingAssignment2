#
# An enhanced matrix object that can store and retrieve its own inverse
#  

#
# makeCacheMatrix creates an enhanced matrix which has the capabiity
# of storing its own inverse
#
makeCacheMatrix <- function(m = matrix) {
  # Assign the passed in matrix to 'm', and set 'inverse' to null
  
  inverse <- NULL
  
  # Assign a new matrix and clear the 'inverse'
  set <- function(y) { 
    m <<- y
    inverse <<- NULL
  }
  
  # Return the original matrix 
  get <- function() m 
  
  # Solve the matrix and assign to 'inverse'
  setinverse <- function(solve) inverse <<- solve 
  
  # Return the 'inverse' (will be null if not yet solved)
  getinverse <- function() inverse
  
  # Assign the function names to a list for execution
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#
# cacheSolve will solve and set the inverse for a given 'makeCacheMatrix' object,
# or will use the already computed result cached in the object to save time.
#

cacheSolve <- function(m, ...) {
  # First retrieve the held inverse figure
  inverse <- m$getinverse()
  
  # Test the result, if it is not null then the result is already cached
  if(!is.null(inverse)) {
    message("Using cached data")
    return(inverse)
  } else {
  # Otherwise get the matrix and solve it
    message("Calculating inverse")
    data <- m$get()
    inverse <- solve(data)
  # Lastly write the matrix inverse in the cache using the set function
    m$setinverse(inverse)
    inverse
  }
}
