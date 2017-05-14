## R Programming - Cache matrix assignment

## makeCacheMatrix:
## This code is based on the vector example posted for 
## this assignment. First step is to create a function to cache the inverse of
## a matrix that will be defined by the end user.

makeCacheMatrix <- function(x = matrix()) {
  # The object of this function consist of an object of 4 functions, list type:
  # set matrix, get matrix, set inverse, get inverse.
  
  # Set inverse to NULL, unless user sets value
  inv <- NULL
  
  # set the matrix, not the inverse yet
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # Set the inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinv <- function() inv
  
  # Save into a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)	
}

## cacheSolve:
## Once we have the matrix ready, we use cacheSolve to compute the inverse 
## and cache the result

cacheSolve <- function(x, ...) {
  ## This functions  will return the inverse of a matrix (inverse of x)
  
  # Get state of inv, check if it´s been computed
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    
    message("Obtaining cached matrix")
    return(inv)
  }
  
  # Get the matrix 
  data <- x$get()
  
  # Get inverse
  inv <- solve(data, ...)
  
  # Cache result
  x$setinv(inv)
  
  # Return inverse
  inv    
}