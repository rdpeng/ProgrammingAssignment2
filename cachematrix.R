## A pair of functions (makeCacheMatrix & cacheSolve) that cache the inverse of a matrix.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## This sets the matrix inverse to null.
  i <- NULL
  
  set <- function (y = matrix()) {
    x <<- y
    
    ## Resets matrix inverse to null, in case matrix was changed.
    i <<- NULL
  }
  
  get <- function () x
  
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  
  
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## cacheSolve computes the inverse of the special "matrix" created in makeCacheMatrix. If the inverse has already been calculated, it retrives it from the cache.

cacheSolve <- function(x, ...) {
  
  ## Returns the cached inverse, if it already exists.
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting matrix inverse from cache")
    return(i)
  }
  
  ## If inverse is null, it calculates it using solve()
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  
  i  
}
