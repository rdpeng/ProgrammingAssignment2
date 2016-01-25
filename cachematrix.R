## This pair of function will take a Matrix input and cache it. Then it will use the cached matrix
## and return its inverse with the Solve Function

## This function takes a matrix and caches it creating a list of functions for the next function to use

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(a) {
    x <<- a
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##  This function uses the matrix cached by the previous function, applies the solve function to create
## the inverse matrix, then returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
