## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  x <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(x)
  
  ## Return the matrix
  x
}
