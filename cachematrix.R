## Objective: create two function to optimize the inverse matrix calculation

## The first one, named 'makeCacheMatrix', stores the given matrix and return a list containing
## The matrix itself, it's inverse matrix and two other functions to get those values.

## The second function, named 'cacheSolve', does the inverse calculation. First, it searchs
## for the inverse matrix. If the calculation were made before then the function doesn't
## make the calculation but it returns the stored inverse matrix.
## If the given matrix is a new one, then this function does the calculation and stores the
## results for future reference.

makeCacheMatrix <- function(x = matrix())
{
## This function create a list with:
## 1-) A function, named 'setMatrix', which stores the matrix value
## 2-) A function, named 'getMatrix', which returns the matrix value
## 3-) A function, named 'setInverseMatrix', which stores the inverse matrix value
## 4-) A function, named 'getInverseMatrix', which returns the matrix value

  ## Starts the inverse matrix with NULL, because for new matrices (for example 'matrix')
  ## the returned list will be like (matrix, get(), null, getinverse())
  inverse <- NULL
  
  ## Function that stores a new matrix, aka a matrix whose inverse matrix
  ## has not been calculated yet
  setMatrix <- function(matrix)
  {
    x <<- matrix
    inverse <<- NULL
  }
  
  ## Function to return the matrix itself
  getMatrix <- function() x
  
  ## Function to store the inverse matrix
  setInverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix
  
  ## Function to return the inverse matrix that has been calculated before
  getInverseMatrix <- function() inverse
  
  ## return the list containing the matrix, inverse matrix
  ## (that will be NULL in case of a new matrix), and the two functions to get the matrix value
  ## and the inverse matrix value
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x, ...)
{
## This function returns the inverse of the matrix 'x'
## Before making calculations, it will search for previous matrix which had their
## inverse matrix calculated before
## In case the given matrix didn't have the inverse matrix stored
## then the 'solve' function will be called
  
  ## Get the inverse of the given matrix
  inverse <- x$getInverseMatrix()

  ## Testing the inverse matrix: in case the inverse were calculated before, return it
  ## Otherwise, call the 'solve' functio to make the calculation
  if(!is.null(inverse))
  {
    message("Getting Cached Matrix")
    return(inverse)
  }

  ## Making the inverse calculation, and stroring the given matrix
  ## and it's inverse for future reference
  newMatrix <- x$getMatrix()
  inverse <- solve(newMatrix, ...)
  x$setInverseMatrix(inverse)
  
  ## Returnig the inverse matrix
  inverse
}