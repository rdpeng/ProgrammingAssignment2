## creates a cacher which retrieves the inverse of matrix if already
## stored otherwise it calculates it on the fly

## makeCacheMatrix creates 4 functions which are used to store
## and retrieve the matrix. Additionally, it helps in storing and
## retrieving the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- mean
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## checks to see if the cache is available. if it is available,
## it returns the cache. otherwise, solves the matrix on the fly

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse()
  i  
}
