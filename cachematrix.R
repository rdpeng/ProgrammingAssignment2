## makeCacheMatrix creates a list of three elements.
##Two Matrices i.e the original and its calcualted inverse matric respectively as well as the function to set the inverse matrix
## list of functions nested within makeCacheMatrix:
### inputMatrix : gets the original matrix value and empties the inversion matrix element 
### origMatrix: returns original matrix 
### setInverse: Sets the inverse matrix from a parameter
### getInverse: returns a matrix as defined by the setInverse value


## makeCacheMatrix caches the inverse matrix of any invertable square matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <-NULL
  
  inputMatrix <- function(y) {
    x         <<- y
    invMatrix <<-NULL
  } 
  
  origMatrix <- function() x 
  setInverse <- function(inverCalculation)  invMatrix <<- inverCalculation
  getInverse <- function() invMatrix
  
  list(inputMatrix = origMatrix, getInverse = getInverse,setInverse= setInverse)
  
  
}


## cacheSolve calculates the inverse matrix and returns a list of 3 elements. Namely 
## Two Matrices i.e the original and its calcualted inverse matric respectively 
## as well as the function to set the inverse matrix value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  
  ## Check if inverse matric already cached. If yes return cached value
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  } 
  
  ## Check if inverse matrix already cached. If 'no' calculate and populate the elements of makeCacheMatrix type vector
  ## get the input matric
  inputMatrix <- x$inputMatrix()
  ## invert matrix
  inverCalculation <- solve(inputMatrix)
  ## populate calculated value
  x$setInverse(inverCalculation)
  
  inverse
  
}
