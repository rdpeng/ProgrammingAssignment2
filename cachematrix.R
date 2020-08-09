
##caching the inverse of a matrix
##make CacheMatrix will create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    get <- function() x
    setinverse <- function(inverse) k <<- inverse
    getinverse <- function() k
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
  
  ##retrieve inverse from the cache
  ##makeCacheMatrix returns a special matrix, cacheSolve function will return its inverse
  
  cacheSolve <- function(x, ...) {
    k <- x$getinverse()
    if (!is.null(k)) {
      message("getting cached data")
      return(k)
  }
    data <- x$get()
    k <- solve(data, ...)
    x$setinverse(k)
    k
  }
