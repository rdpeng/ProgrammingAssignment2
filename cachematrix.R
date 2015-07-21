## makeCacheMatrix -- defines a list of functions that 
##    allows storage of a particular matrix's inverse
## cacheSolve -- accepts a 'makeCacheMatrix' and either
##    returns its chached inverse or calculates & stores the inverse

## CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(y) inv <<- y
  getinv <- function() inv
  list(set=set, get=get,
       setinv=setinv, 
       getinv=getinv)
}


## Solve the CacheMatrix object's inverse & store it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinv(inv)
  inv
}
