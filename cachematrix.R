# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
  
  
  setMa <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMa <- function() {
    x
  }
  
  # cache the given argument 
  cacheInv <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInv <- function() {
    cache
  }
  
  list(setMa = setMa, getMa = getMa, cacheInv = cacheInv, getInv = getInv)
}



cacheSolve <- function(h, ...) {

  inverse <- h$getInv()

  if(!is.null(inverse)) {
    message("getting cached data for review")
    return(inverse)
  }

  data <- h$getMa()
  inverse <- solve(data)
  h$cacheInv(inverse)
  
  inverse
}

# Esteban Vargas B
