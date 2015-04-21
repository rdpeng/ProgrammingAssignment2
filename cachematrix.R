## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(my_mat = matrix()) {
  
  ## initialize my_mat to empty matrix
  ## inv is the inverse of the my_mat; 
  ## set inv to NULL at the start
  
  inv <- NULL
  
  ## This is the set function, use this to assign a matrix
  ## Set "inv" to NULL as my_mat has changed
  setmatrix <- function(y) {
    my_mat <<- y
    inv <<- NULL
  }
  
  ## The access function is named 'get'
  getmatrix <- function() my_mat
  
  ## Assign the inverse of the matrix
  setinverse <- function(inv_mat) inv <<- inv_mat
  
  ## Access function to retrieve the inverse of the matrix
  getinverse <- function() inv
  
  list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Find out if the cached copy is already presnt.
  ## If present then return the invers 'inv'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## inverse is not yet calculated, need to do it now. 
  ## First get the matrix to 'data'
  data <- x$getmatrix()
  inv <- solve(data, ...)   ## use solve to get the inverse of the matrix
  
  ## USe the setinverse function to assign a value to the inv
  x$setinverse(inv) 
  
  ## Return the inverse
  inv
}
