## With this function we calculate the inverse of a square Matrix.
# In order to avoid calculating the cachematrix over and over 
# every time we run the makeCacheMatrix we should have to separate
# both functions.

## With this makeCacheMatrix we get the matrix, find the inverse with
# the Solve() function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function is called in the makeCacheMatrix function
# Its main purpose is to avoid calculate the inverse of a matrix
# again when it's supposed to be stored in memory. Making the 
# algorithm more efficient.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}