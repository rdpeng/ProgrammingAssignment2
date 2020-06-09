# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
# solve function in R can be used to compute the inverse of square matrix.


##  Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## To set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## To get the matrix
  get <- function() x
  ## To set the inverse of the matrix
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   ## To return a list of the methods
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"  created by makeCacheMatrix above.
##If the inverse has already been calculated (and the 
# matrix has not changed), then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## Return the inverse if its already set
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Get the matrix from the object
  mat <- x$get()
  ## Calculate the inverse
  inv <- solve(mat, ...)
  ## Set the inverse of the object
  x$setInverse(inv)
  ## Return the matrix
  inv
}

