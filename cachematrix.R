## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverted) inverse <<- inverted
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}

# Here I create a 2*2 matrix to test my functions

mymatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

# To get the matrix 2*2
mymatrix$get()
# to calculate the inverted matrix
cacheSolve(mymatrix)

#to get the inverted matrix
mymatrix$get.inverse()
