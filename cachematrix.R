## -------------------------------------------------------------
## Pair of functions that cache the inverse of a matrix:
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
##    retrieve the inverse from the cache.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function(x0){
    x <<- x0
    inversion <<- NULL
  }
  get <- function() x
  setInv <- function(inv) inversion <<- inv
  getInv <- function() inversion
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversion <- x$getInv()
  if (!is.null(inversion)){
    message("Getting cached data...")
    return(inversion)
  }
  else{
    message("Creating cached data...")
    ## Calculate an inversion of a matrix: x$setInv(solve(x$get()))
    inversion <- solve(x$get())
    x$setInv(inversion)
    return(inversion)
  }
}
