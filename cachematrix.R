##  pair of functions that cache the inverse of a matrix.

## set x as an input matrix
## set the solved value "j" as a null

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setsolve <- function(solvemat) j <<-solvemat
  getsolve <- function() j
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## compute inverse of matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  j <- x$getsolve()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  data <- x$get()
  j <- solve(data)
  x$setsolve(j)
  j
}
