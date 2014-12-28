## This pair of functions will cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<-inverse
  getinverse <-function() s
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.

cacheinverse <- function(x,...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message('getting cache data')
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setinverse(s)
  s
}
