## An R function that is able to cache potentially time-consuming computations

#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
#' examples
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' x$set(matrix(rnorm(16), 4, 4))
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

#' calculates the Inverse of a matrix created with the above function. 
#' it first checks to see if the inverse has already been calculated. 
#' If so, it gets the inverse from the cache and skips the computation.
#' @param x the result of a previous makeCacheMatrix call
#' @param ... additional arguments to pass to solve function
#' examples
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
        
    inverse <- x$getInverse()
    if(!is.null(inverse)){
      message("getting chached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse ## Return a matrix that is the inverse of 'x'
}


