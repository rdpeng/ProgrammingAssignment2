makeCacheMatrix <- function(matrix) {
  inv <- NULL
  
  set <- function(newValue) {
    matrix <<- newValue
    inv <<- NULL  # Reset cached inverse
  }
  
  get <- function() {
    matrix
  }
  
  cacheInverse <- function() {
    if (!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    }
    
    inv <<- solve(matrix)
    inv
  }
  
  list(set = set, get = get, cacheInverse = cacheInverse)
}
cacheSolve <- function(cacheMatrix) {
  if (!is.function(cacheMatrix$cacheInverse)) {
    stop("Input must be a cacheMatrix object.")
  }
  
  cacheMatrix$cacheInverse()
}