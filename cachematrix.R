## caches the inverse matrix of a given matrix

## Creates a vector (list) that acts as the caching element

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invmat) m <<- invmat
  getinverse <- function() m
  list(set = set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Either retrieves the cached inverse or calculates it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(x, ...)
        x$setinverse(m)
        m
}
