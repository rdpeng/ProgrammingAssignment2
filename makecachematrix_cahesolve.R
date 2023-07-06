makeCacheMatrix <- function() {
  mat <- NULL
  inv <- NULL
  
  set <- function(x) {
    mat <<- x
    inv <<- NULL  
  }
  
  get <- function() {
    mat
  }
  
 setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(cacheMatrix) {
  mat <- cacheMatrix$get()
  
  inv <- cacheMatrix$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  inv <- solve(mat)
  
  cacheMatrix$setInverse(inv)
  
  inv
}
