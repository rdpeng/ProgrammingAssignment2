## Put comments here that give an overall description of what your
## functions do

## There are two functions here; MakeCacheMatrix and cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #Initializing the inverse as NULL
  # Setting the matrix
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
   # Getting the matrix
  get <- function() x
  #Setting the inverse
  setInverse <- function(inverse){ 
          inv<<-inverse
          }
  #Getting the inverse
  getInverse <- function(){
          inv <- ginv(x)
          inv%*%x      #function to obtain inverse
  }
  #returns a list of defined functions
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## Write a short comment describing this function
## This is used to get the CacheSolve
cacheSolve <- function(x, ...) {
         # retrieves the matrix of the "matrix" object passed as an argument
  mat <- x$get()
  inv <- x$getInverse()    # checks if the reverse is already cached
  if (!is.null(inv)) {
    message("Retrieve the inverse from the cache")
    return(inv)
  }
  
  # otherwise, calculate the inverse of the matrix
  inv <- solve(mat, ...)
  
  # caches the reverse
  x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
