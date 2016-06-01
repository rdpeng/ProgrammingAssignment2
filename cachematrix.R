## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  nulvar <- NULL
  set <- function(y) {
    x <<- y
    nulvar <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) nulvar <<- inverse
  getInverse <- function() nulvar
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  nulval <- x$getInverse()
  if (!is.null(nulval)) {
    message("getting cached data")
    return(nulval)
  }
  mat <- x$get()
  nulval <- solve(mat, ...)
  x$setInverse(nulval)
  nulval
}
