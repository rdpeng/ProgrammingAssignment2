## The below functions create an invertible matrix and make the inverse 
## of the matrix available in cache.

## makeCacheMatrix creates and returns a list of set and get functions
## used by cacheSolve to get or set the inverted matrix in cache.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix created in the makeCacheMatrix 
## function. If the inverted matrix does not exist in cache, it is created in the 
## working environment and stored in cache. It it already exists, it is returned.

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
