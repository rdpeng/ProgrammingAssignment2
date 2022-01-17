## Here we have created two functions namely 'makeCacheMatrix' and 
## 'cacheSolve' which basically can be worked out in unision so as 
## to cache the inverse of a matrix and help us save computational 
## time 

## Below is the 'makeCacheMatrix' which creates a special "matrix" 
## object that can cache its own inverse.  The inverse matrix is cached 
## inside the variable inv, within the main environment,
## which is unique for each iteration the function is called.
## The output of the ;makeCacheMatrix is a list generated with four elements, which are 
## the four functions defined herein: set, get, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Below is the 'cacheSolve' which computes the inverse of the special "matrix" returned by 
## the 'makeCacheMatrix' function above. If the inverse has already been calculated 
## (and the matrix has not changed), then this function will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
