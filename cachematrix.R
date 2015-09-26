## This is an exercise of matrix inversion for the R programming course.
## The functions below are intended to work in pair.

## The first function creates a matrix object capable of caching its inverse
## It is made of 4 functions: to define or get the matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(matrixNew) {
    x <<- matrixNew
  }
  
  get <- function() {
    print(x)
  }
  
  makeInverse <- function(x) {
    inverse <<- solve(x)
  }
  
  getInverse <- function() {
    print(inverse)
  }
  
  list(set = set,
       get = get,
       makeInverse = makeInverse,
       getInverse = getInverse)
}


## The function below print the inverse of the matrix.
## The computation is avoided if already performed.

cacheSolve <- function(x, ...) {
  if(x$getInverse != NULL) {
    inverse <- makeInverse(x)
  }
  print(inverse)
}
