## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix takes a matrix as an argument.
  # makeCacheMatrix initializes inv to NULL.
  
  inv <- NULL
  
  # setinv sets the inverse value of matrix using solve function if the argument 
  # is a valid matrix.
  
  setinv <- function()
    (if (is.matrix(x))
    {
      inv <<- solve(x)
    })
  
  # getInv function gets the inverse matrix created using the Setinv function
  getinv <- function()
    inv
  
  # getmatrix function returns the argument matrix x
  getmatrix <- function()
    x
  
  # return the list of functions back.
  list (setinv = setinv, getinv = getinv, getmatrix = getmatrix)
  
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # this function will call the getinv function and check if inverse exists.
  inv <<- x$getinv()
  
  # if inverse does not exists, it will create inverse
  if (is.null(inv))
  {
    newx <- x$getmatrix()
    inv <<- x$setinv()
  }
  else
  {
    print("getting from cached data")
  }
  #return inverse
  inv
}
