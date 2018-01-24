## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special matrix, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix 
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) 
    i <<- inverse
  getInverse <- function() i
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

## Write a short comment describing this function
## The following function calculates the inverse of the special matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}
