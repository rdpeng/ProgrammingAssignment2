## Return a matrix that is the inverse of 'x'


## The first function, makeCacheMatrix creates a list containing a function to
## * get the matrix
## * set the inverse (with the function solve)
## * get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## this function also check if the inverse has already been calculated (and the matrix has not changed)
## in this case, cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  inverse <- x$getsolve()
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}

