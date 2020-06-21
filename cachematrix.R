

## It's a function that gets a vector and sets its mean value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
   get <- function() x
    setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If the mean has not been already calculated, this function gives the mean. Otherwise, the cached mean is used.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}