#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  #get the value of the matrix
  getMatrix <- function() x 
  #Set the value of the invertible matrix
  setInverse <- function(inverse) matrixInverse <<- inverse 
  #get the value of the invertible matrix
  getInverse <- function() matrixInverse
  #create a list
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  #get the value of the invertible matrix from the makeCacheMatrix function
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {                      #if inverse matrix is not NULL
    message("getting cached data")
    return(matrixInverse)
  }
  #if value of the invertible matrix is NULL then 
  data <- x$getMatrix()
  #use solve function to inverse the matrix
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}
