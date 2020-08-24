## Below are two functions that are used to create a special object that stores a matrix and caches the inverse matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## inv_matrix is storing the inverse matrix. 


makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_matrix <<- inverse
  getInverse <- function() inv_matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInverse()
  if(!is.null(inv_matrix)) {
    message("getting cached Matrix")
    return(inv_matrix)   
  }
  matrix_data <- x$get()
  inv_matrix <- solve(matrix_data, ...)
  x$setInverse(inv_matrix)
  inv_matrix  
}
