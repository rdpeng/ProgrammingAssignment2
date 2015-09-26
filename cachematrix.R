## The following pair of functions ("makeCacheMatrix" and "cacheSolve") caches 
## the invese of a matrix specified by user.
## This pair of function is one of the solutions which improves times and 
## the cost of the hardware used for matrix inversion.

## The "makeChacheMatrix" function stores the inverse of a matrix created 
## in the solve function.
## This also returns a list of function used in the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
  ## Make the cached inverse matrix NULL to initialise  
  cachemx <- NULL
  
  ## Set a matrix in the working environment
  set <-function (y) {
    x<<-y
    cachemx <<- NULL
  }
  
  ## Get the matrix value
  get <- function () x
  ##  Set the inverse of the matrix and stores in cache
  setInverse <-function(inverse) cachemx <<-inverse
  ## Get the inverse of the matrix
  getInverse <-function () cachemx
  
  ## Return the above functions to the working environment 
  list (set = set, get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}
