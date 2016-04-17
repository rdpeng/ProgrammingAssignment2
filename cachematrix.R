## The following set of functions will cache the inverse of a matrix.



## the makeCacheMatrix fuction will:
## the function will do the following
## Set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix



makeCacheMatrix <- function(x = matrix()) {
  Invrs <- NULL
  set <- function(y) {
    x <<- y
    Invrs <<- NULL
  }
  get <- function() x
  setInvrs <- function(Inverse) Invrs <<- Inverse
  getInvrs <- function() Invrs
  list(set = set, get = get,
       setInvrs = setInvrs,
       getInvrs = getInvrs)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  Invrs <- x$getInvrs()
  if(!is.null(Invrs)) {
    message("getting cached data")
    return(Invrs)
  }
  NewCheck <- x$get()
  Invrs <- solve(NewCheck, ...)
  x$setInvrs(Invrs)
  Invrsinvr
}



