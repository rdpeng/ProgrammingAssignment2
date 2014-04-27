## Functions makeCacheMatrix and cacheSolve work together to find the inverse of an input matrix 
## by either retrieveing tte inverse from the cache, if avaialble, or calculating the inverse and then caching it.

## Creates a list of functions that can be used to get/set the input matrix and get/set the inverse of the input matrix.

makeCacheMatrix <- function(inpMatrix=matrix()) {
  inverseMatrix <- NULL
  setStoredMatrix <- function(tempInpMatrix) {
    storedMatrix <<- tempInpMatrix
    inverseMatrix <<- NULL
  }
  getStoredMatrix <- function() storedMatrix
  setInverse <- function(inpInverse) inverseMatrix <<- inpInverse
  getInverse <- function() inverseMatrix
  list(setStoredMatrix = setStoredMatrix, getStoredMatrix= getStoredMatrix, setInverse= setInverse, getInverse = getInverse) 
}


## Checks whether the inverse of the input matrix is available in the cache. If yes, then it returns the inverse stored in the cache, else calculates inverse, stores it in the cache and returns the inverse.

cacheSolve <- function(inpMatrix, ...) {
 
  inverseMatrix <- inpMatrix$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  tempInpMatrix <- inpMatrix$getStoredMatrix()
  inpInverse <- solve(tempInpMatrix)
  inpMatrix$setInverse(inpInverse)
  inpInverse
}
