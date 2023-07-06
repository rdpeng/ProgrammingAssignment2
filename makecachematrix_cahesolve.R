makeCacheMatrix <- function() {
  # Initialize the matrix and cache variables
  mat <- NULL
  inv <- NULL
  
  #set the matrix
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
  
 #retrieve the matrix and set the inverse 
  getInverse <- function() {
    inv
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(cacheMatrix) {
  # Retrieve the matrix
  mat <- cacheMatrix$get()
  
  inv <- cacheMatrix$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  # Calculate the inverse
  inv <- solve(mat)
  
  # Cache the inverse
  cacheMatrix$setInverse(inv)
  
  inv
}
