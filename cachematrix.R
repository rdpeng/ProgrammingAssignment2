## This makeCachMatrix take a matrix as x into the function and create an inverse matrix of x
## The function contains sub-functions which are 
## set() to set matrix 'x' from external environment
## get() to let external environment get the matrix 'x'
## setInvert() to set inverse matrix 'x' from external environment
## getInvert() to let external environment get the inverse matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) inv <<- solveMatrix
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function return a matrix that is the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
  ## Call the function getInv() to get the inverse matrix and assign it to 'inv'
  inv <- x$getInv()
  
  ## Check the result from the previous action. 
  ## If the variable inv (inverse matrix) is not null, then return 'inv'
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## If the 'inv' is null, run the function get() 
  ## to assign the original matrix to variable 'matrix'.
  ## Then, set the inverse matrix using solve() and assign it to inv
  ## Last step, call the function setInv() to cache the inverse matrix to makeCacheMatrix()
  ## for next time call.
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInv(inv)
  return(inv)      
}

