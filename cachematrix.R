## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  
  set <- function(matrix) {
    mat <<- matrix()
    i <<- NULL
  }
  
  get <- function() {
    mat
  }
  
  setInverse <- function(inv) {
    i <<- inv
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mat <- x$getInverse()
  
  if(!is.null(mat)) {
    message('Getting cached data')
    return(mat)
  }
  data <- x$get()
  mat <- solve(data) %% data
  
  x$setInverse(mat)
  
  mat
}
