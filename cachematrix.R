## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A pair of fnxs that cache the matrix transposition (inverse)
## Creation of a special matrix object that can cache its transposed function (inverse)
makeCacheMatrix <- function(x = matrix()) {
  ## Creating the inverse property (inv)
  inv <- NULL
  
  ## Set as an object to become a matrix
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
}
        
  ## Functuon to get matrix "m"
  get <- function() {
    ##Use the 'm' object
    m
  }

        
 ## Function to set Inverse of Matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
 ## Function to set Inverse of Matrix
  getInverse <- function() {
    ##Use Inverse Function
    inv
  }

        
 ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ##Only return the inverse of matrix its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
}

  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
