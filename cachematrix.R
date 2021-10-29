   
## Pair of functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 

#' Util function that set the matrix and the inverse in an environment
#' parameter -  g an invertible matrix
#' examples
#' g = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' g$set(matrix(rnorm(16), 4, 4))



makeCacheMatrix <- function(g = matrix()) {
  # todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
    g <<- y
    inv <<- NULL
  }
  get <- function() g
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,setinv = setinv, getinv = getinv)
}


#' Compute and cache the inverse of a matrix
#' Parameter - g is the result of a previous makeCacheMatrix call
#' Parameter -  ... additional arguments to pass to solve function
#' examples
#' g = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(g)



cacheSolve <- function(g, ...) {
  ## Return a matrix that is the inverse of 'g'
  inv <- g$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- g$get()
  inv <- solve(data, ...)
  g$setinv(inv)
  inv
}
