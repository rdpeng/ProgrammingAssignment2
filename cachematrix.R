## Put comments here that give an overall description of what your
## functions do

## caches a matrix and returns a list of getters and setters for its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calculates the inverse of a cached matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m))
    {
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
