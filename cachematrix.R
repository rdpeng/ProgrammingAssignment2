## Matrix inversion is usually a costly computation
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below functions create special "matrix" object and cached the inverse of the matrix 

## This function creates a special "matrix" object. This matrix object able to cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseOfMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverseOfMatrix <<- inv
  getinverse <- function() inverseOfMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns an inverse of the matrix created by makeCacheMatrix function
## If the inverse already calculate once then it will retrieve from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
