## Put comments here that give an overall description of what your
## functions do

## This is a function that saves an inputted matrix as object

makeCacheMatrix <- function(x = matrix()) { ## Defines function
  ## that takes a matrix as an argument
  inverse_matrix <- NULL ## Sets default value of matrix object inverse_matrix to NULL
  set <- function(y) { ## Defines set as a function on object y
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() {x} ## Obtains value of input matrix
  set_inverse <- function(solve) {inverse_matrix <<- solve} ## Solves
  ## for inverse of square matrix object x; won't work if the 
  ## matrix is not square
  get_inverse <- function() {inverse_matrix} ## Obtains value of 
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
  ## Stores output as a list
}

## A function that calculates the inverse of the matrix in
## makeCacheMatrix. Checks if there is a cache of the inverse,
## and retrieves it to the console

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$get_inverse()
  if(!is.null(inverse_matrix)) { ## Checks if inverse is
    ## already calculated
    message("Retrieving cached data...") ## Only prints if
    ## there is cached information on matrix object in argument
    return(inverse_matrix) ## Prints the values of inverse_matrix
    ## to the console
  }
  data <- x$get() ## Defines data as an object retrieved from 
  ## data value 
  inverse_matrix <- solve(data, ...) ## solves the inverse of 
  ## matrix from object named data
  x$set_inverse(inverse_matrix)
  inverse_matrix ## Prints value of inverse_matrix to console
}
