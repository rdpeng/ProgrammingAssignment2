## x is a square matrix which is presumed to be invertible.
## out is a list containing the input matrix to cacheSolve:
## set = 

makeCacheMatrix <- function(x = matrix()) {
  #initialize inv
  inv <- NULL

  set <- function(y) {
    ##Pull x and inv from a different environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv) 
}

