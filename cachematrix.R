## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix 
## set the value of the matrix inverse 
## get the value of the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) inverse <<- solveMatrix
  getInv <- function() inverse
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return inverse of 'x' as a matrix
  inverse <- x$getInv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  Temp <- x$get()
  inverse <- solve(Temp)
  x$setInv (inverse)
  inverse
}
