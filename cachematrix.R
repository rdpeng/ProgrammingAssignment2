## Author :  Enes Kemal Ergin
## Date   :  01/23/2015 

"""
##Instructions
--------------
Write the following functions:

    makeCacheMatrix: This function creates a special 'matrix' 
                      object that can cache its inverse.
    cacheSolve: This function computes the inverse of the special 'matrix'
                returned by makeCacheMatrix above. If the inverse has already 
                been calculated (and the matrix has not changed), then cacheSolve 
                should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the solve function in R. 
For example, if X is a square invertible matrix, then solve(X) returns its inverse.

"""
makeCacheMatrix <- function(x = matrix()) {
    
  inv <- NULL ## Assigns the inverse of matrix's value
  
  ## This function sets the values of the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## This function gets the value of the matrix
  get <- function() x 
  
  # set the value of the inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  
  ## get the value of the inverse
  get_inverse <- function() inv
  
  ## return a list of all the above functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## This function checks if the inverse is already cached first, if 
  ## it is already a inverse it gets the inverse 
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("Getting cached inverse...")
    return(inv)
  }
  
  ## Otherwise get the matrix using x$get
  data <- x$get()
  
  ## then calculate the inverse
  inv <- solve(data, ...)
  
  ## then cache the inverse of the matrix
  x$set_inverse(inv)
  
  ## Returns the result
  inv
}
