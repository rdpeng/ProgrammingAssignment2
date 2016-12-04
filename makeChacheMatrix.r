# The following function returns the inverse of the matrix. First, it check to see if the inverse
# has already been computer. If true, then it pulls the result and skips the rest of the code.
# If not, it calculates the inverse and uses the setinverse function to establish the value
# in the cache.

cacheSolve <- function(mtrx, ...) {
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {
  message("Getting cached data...")
  return(inverse)
  }
  data <- mtrx$get()
  invserse <- solve(data, ...)
  mtrx$setinv(inverse)
  return(inverse)
}

