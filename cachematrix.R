
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # cached value of the inverse of the matrix
  
  ## set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset the cached inverse
  }
  
  ## get the value
  get <- function() x
  
  ## set the inverse
  setinv <- function(i)  inv <<- i
  
  ## get the inverse
  getinv <- function() inv
  
  ## return a list of the functions for the cacheMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x', using the cache if possible
cacheSolve <- function(x, ...) {
  ## first check to see if the inverse has already been calculated
  i <- x$getinv()
  if(!is.null(i)) {
    # already calculated - just return the cached value
    message("getting cached")
    return(i)
  }
  
  # else we need to solve() 
  data <- x$get() # get the actual matrix value, not the wrapper list of functions
  i <- solve(data)
  
  # now set the inverse 
  x$setinv(i)
  
  #return the inverse
  i
}