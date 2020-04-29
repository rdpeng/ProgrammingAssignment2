## The following functions help in generating a matrix and cache
## its inverse

## This function generates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  X <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function caches inverse of matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m                ## Return a matrix that is the inverse of 'x'
}
