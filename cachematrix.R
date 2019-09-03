## The cachematrix.R file checks to see that an inverse is made.  If there is, it returns it.  If not it makes and caches it.

## This function creates a special list which contains a function to set the value of the vector, get the value of the vector, set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the m is null.  If not, it returns the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
