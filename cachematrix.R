## The first function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Then the function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.

## The function makeCacheMatrix creates am trix object. This object is capable of caching its own inverse.

makeCacheMatrix <- function(x = matrix()) {
         j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)

}


## The function cachezsolve here computes the inverse of the matrix object that is returned from the above function.
## Also, if the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data") 
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
}
