## Put comments here that give an overall description of what your
## functions do

## This function creates a Cache Matrix using a line of thinking similar to the example

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set=set, get = get, 
       setInv = setInv,
       getInv = getInv)
}


## This function checks to see if a matrix has been inverted and cached. If it has, it returns the cached solution
## If it hasn't, it inverts the new matrix, caches the solution, and returns the new inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m              ## Return a matrix that is the inverse of 'x'
}
