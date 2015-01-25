##get the inverse of a square matrix from cache if has been calculated before or calculate if not

## get the inverse matrix and store in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  
  m <- x$getinverse()
  if(!is.null(m)) { ##Search if the matrix is already in cache
    message("getting cached data")
    return(m)
  }
  data <- x$get() ##if not calculates it
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
