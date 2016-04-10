# Week 3 (Assignment-2, Lexical Scoping)
# Goal of the assignment - >> "Write a pair of functions that cache the inverse of a matrix". In this assignment it is given
# that input matrix should be invertible (inverse exist) i.e determinant(matrix) != 0. 

# Defined a function called "makeCacheMatrix" which creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  # Assigned matrixinverse = Null in this environment using assignment operator "<-"
  matrixinverse <- NULL
  set <- function(y) {
    # Assigned x = y in global environment using superassignment operator "<<-" 
    x <<- y
    # Similarly, assigned matrixinverse = Null in global environment using superassignment operator "<<-"
    matrixinverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrixinverse <<- inverse
  getInverse <- function() matrixinverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Defined a function "cacheSolve" which computes the inverse of the special "matrix" returned by makeCacheMatrix above.If the inverse has 
# already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getInverse()
  ## If matrixinverse in Null then return data stored in matrixinverse 
  if (!is.null(matrixinverse)) {
    message("--> Importing cached data")
    return(matrixinverse)
  }
  mat <- x$get()
  ## Calculation inverse of matrix using solve function if matrixinverse does not exist in cache.
  matrixinverse <- solve(mat, ...)
  x$setInverse(matrixinverse)
  matrixinverse
}