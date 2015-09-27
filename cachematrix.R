## cachematrix.R
## contains two functions for creating and using a 'cachematrix' 
## object - a list that contains a standard matrix, along wirh its 
## calculated solution/inverse; each of which is accessed and 
## manipulated in the cachematrix via wrapper getters and setters

## Makes a 'cached Matrix' actually a list containing a matrix
## and potentially its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #inverse is initially null
  
  ## Sets a new value to the matrix; doing so invalidates any stored inverse
  set <-function(newMatrix) {
    x<<- newMatrix
    inv<<- NULL
  }
  
  ## Returns the stored matrix
  get <- function() {
    x 
  }
  
  ## Sets a value to the inverse matrix
  setInv <- function(newInv) {
    inv <<- newInv
  }
  
  ## Gets the value of the inverse matrix
  getInv <- function() {
    inv
  }
  
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}


## Solve a cachematrix, returning its inverse if already known
## or determining it, caching it, and returning it if not 
cacheSolve <- function(x, ...) {
        
        ## Check if this has already been solved and cached
        cachedSolution <- x$getInv()
        if (!is.null(cachedSolution)) {
          message("cached solution found, returning that")
        
          return(cachedSolution)
        
        } else {
          ## No cached solution, so solve and cache
          message("no cached solution found, solving matrix")
          matrixToSolve <- x$get()
          
          ## EXTEND - this assignment assumes the matrix is always solveable;
          ## in a real-word scenario, we'd need to check that here.
          solvedMatrix <- solve(matrixToSolve)
          x$setInv(solvedMatrix)
          
          return(solvedMatrix)
        }
}
