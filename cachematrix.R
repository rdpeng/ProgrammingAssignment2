## This function returned the inverse of the matrix x

## makeCacheMatrix creates a special matrix
## first, this function gets the matrix
## second it sets the inverse using the function solve
## finally  it gets the inverse

makeCacheMatrix <- function(x = matrix()) {

  matrixinve <- NULL
  get <- function() x
  setmatrix <- function(solve) matrixinve <<- solve
  getmatrix <- function() matrixinve
  list(get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## cacheSolve, calculates the inverse of matrix returned by makeCacheMatrix
## the function also check if the inverse is available, cachesolve retrieves it
## in the other case the cacheSolve computes, caches and returnes the inverse of the matrix 


cacheSolve <- function(x, ...) {
        matrixinve <- x$getmatrix ()
        if(!is.null(matrixinve)) {
          message("getting cached inverse")
          return(matrixinve)
        }
        matrixinve <- x$getmatrix()
        data <- x$get()
        matrixinve <- solve(data, ...)
        x$setmatrix (matrixinve)
        matrixinve
}
