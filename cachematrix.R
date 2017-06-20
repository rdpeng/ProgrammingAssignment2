## Put comments here that give an overall description of what your
## functions do
# Usage
# a <- makeCacheMatrix(matrix(sample.int(15, size = 4*4, replace = TRUE), nrow = 4, ncol = 4))
# b <- cacheSolve(a)
# b is the inverse of the matrix

## Write a short comment describing this function
# returns a list with 4 functions . setInverse caches the result in higher environment
#variable m. Get interface returns the original matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function(){ return(m) }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}
## Write a short comment describing this function
#Interface to the cache . If hit, returns the result from cache else calculates inverse, stores
#in the cache and returns the value.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(is.null(m)) {
    m <- solve(x$get())
    x$setInverse(m)
  }
  m
  
}
