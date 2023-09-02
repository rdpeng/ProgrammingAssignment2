## The makeCacheMatrix function caches the matrix given by the argument.
## The cacheSolve function returns the inverse of a matrix given by the argument. 
# Either this is done by finding the inverse matrix in the cache, or it will be computed.

## makeCacheMatrix caches the input matrix.
makeCacheMatrix <- function(x = matrix()) {
  x <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) x <<- solve
  getinverse <- function() x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse matrix of the input matrix.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  solve(x)
}