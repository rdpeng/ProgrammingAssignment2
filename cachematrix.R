## Caching the Inverse of a Matrix

## Creates a special vector which is a list containing functions to :
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the matrix.
## If it has already be computed, retrived it from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
