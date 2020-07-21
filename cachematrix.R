## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initialize cache matrix and set it to null
  cacheMatrix <- NULL
  #method setMatrix()
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  #method getMatrix(), returns: matrix.
  getMatrix() <- function() x 
  #method setCache()
  setCache <- function(inverse) cacheMatrix <<- inverse
  #method getCache()
  getCache <- function() cacheMatrix
  #listing named of methods that will be publicly available
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Checking for content of the cache matrix
  cacheMatrix <- x$getCache()
  #If the content is not null
  if (!is.null(cacheMatrix)) {
    message("Load: cache matrix.")
    return(cacheMatrix)
  }
  #If there is no content, get matrix, create, set, update, return cache matrix
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
