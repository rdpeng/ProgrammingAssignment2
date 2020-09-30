## This function creates a special "matrix" object that can cache its inverse.
## sample is the matrix object that user will submit on the console


makeCacheMatrix <- function(sample = matrix()) {
  invsample <- NULL
  set <- function(x) {
    Sample <<- x
    invsample <<- NULL
  }
  get <- function() sample
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() invsample
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(sample, ...) {
  ## Return a matrix that is the inverse of 'sample'
  inv <- sample$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invsample)
  }
  mat <- sample$get()
  invsample <- solve(mat, ...)
  sample$setInverse(invsample)
  invsample
}
