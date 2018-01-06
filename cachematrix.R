##These two functions cache the inverse of a matrix
##which can prevent potentially very time consuming computations

## define function with matric as default args

makeCacheMatrix <- function(x = matrix()) {
  #initialize null
  inverse <- NULL                           
  
  #assign set, get, setInverse and getInverse functions
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverse <- function() inverse
  #next line allows for $ operator
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes or retrieves the inverse of a matrix depending on whether
## the inverse is cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  #retrieve from cache if available
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
