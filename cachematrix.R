## The function below creates a special "matrix" object that can cache its 
##inverse.

makeCacheMatrix <- function(x=matrix()) {
  matrix <- NULL
  inverse <- NULL
  set <- function(x) {
    matrix <<- x
    inverse <<- NULL
  }
  get <- function() {
    matrix
  }
  getInverse <- function() {
    if (is.null(matrix)) {
      stop("Matrix not set. Use 'set' function to set the matrix.")
    }
    if (is.null(inverse)) {
      message("Calculating inverse\n")
      inverse <<- solve(matrix)
    } else {
      message("Getting cached inverse\n")
    }
    inverse
  }
  list(set = set, get = get, getInverse = getInverse)
}

## The function below computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(matrixCache) {
  matrixCache$getInverse()
}