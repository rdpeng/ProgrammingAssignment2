##This program is responsible for caching the inverse of the matrix, which can then be retrieved for further uses, when required. Caching is required since inverse calculation is a costly operation.

##Creates a special "matrix" object which can cache it's inverse which can then be inverted by cacheSolve function.
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
## The function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
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
