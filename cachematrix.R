## Programming Assignment 2 _ By: Shuang Zhang
## Cache the inverse of a matrix

## This function creates a special ¡°matrix¡± object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  tmp_inverse <- NULL
  set <- function(y){
    x <<- y
    tmp_inverse <<- NULL
  }
  
  get <- function() 
    x
  setInverse <- function(inverse)
    tmp_inverse <<- inverse
  getInverse <- function()
    tmp_inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  tmp_inverse <- x$getInverse()
  if(!is.null(tmp_inverse)){
    message("Getting cached data")
    return(tmp_inverse)
  }
  tmp_x <- x$get()
  tmp_inverse <- solve(tmp_x, ...)
  x$setInverse(tmp_inverse)
  tmp_inverse
}
