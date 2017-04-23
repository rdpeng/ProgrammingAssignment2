## The below two functions are for the Programming assignment 2 of week 3 
## of the datascience course in coursera.The prime purpose of these functions is
## to calculate the inverse of a matrix provided that the matrix is a square matrix.
## As calculating the inverse of a matrix is costly computationally, caching would
## improve performance over repeatedly calculating for the same given matrix.

## makeCacheMatrix creates a matrix and defines the setters and getters
##for the matrix in hand and the cached value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## set the given matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## get the given matrix
  get <- function()
    x
  ## set the calculated inverse of the given matrix
  setinv <- function(inv)
    i <<- inv
  ## get the calculated inverse of the given matrix
  getinv <- function()
    i
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
  
}


## cacheSolve calculates the inverse of the given matrix
##provided that its a square matrix. It first checks for a cached instance
##before actually calculating the inverse

cacheSolve <- function(x, ...) {
  ## Get the cached instance of calculated inverse matrix if any
  i <- x$getinv()
  ## Check if there is any cached instance for the given matrix
  if (!is.null(i)) {
    ## notification message for indicating whether cached instance is available
    message("getting cached data")
    ## returns cached instance of inverse matrix
    return(i)
  }
  ## data gets loaded with the given matrix for which inverse has to be calculated
  data <- x$get()
  ## calculates the inverse of the given matrix x loaded in data
  i <- solve(data, ...)
  ## sets the calculated value so that it can be taken from cache 
  ##for repeated function calls with the same input
  x$setinv(i)
  i
}
