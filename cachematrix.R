## Coursera Programming Assignment2
## These two functions improve the performance of the R code where repeated tasks take longer time
## The functions are used to cache the operation output and return when the operation is repeated



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #Initialize local variable
  inver <- NULL
  ## Set the value of the Matrix, Get the Value of the Matrix
  set <- function(y) {
    ## '<<-' is used to assign the value from a different environment
    x <<- y
    inver <<- NULL
  }
  get <- function() {
    x
  }
  ##Set and get the Inverse of the Matrix
  setSolve <- function(inverse) {
    inver  <<- inverse
  }
  getSolve <- function() {
    inver
  }
  list(
    set = set,
    get = get,
    setSolve = setSolve,
    getSolve = getSolve
  )
}

## Function to check and return the cache if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getSolve()
  ## Check if the inverse has already been calculated, then return the avialable inverse from the cache
  if (!is.null(inver)) {
    ## get cached data and return it
    return(inver)
  }
  ## if the inverse has not already been calculated, then solve the inverse
  mymatrix_data <- x$get()
  inver <- solve(mymatrix_data, ...)
  ## Set cache and return the new inverse
  x$setSolve(inver)
  inver
}
