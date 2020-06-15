## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y   # Note "<<-" assigns value y to object x in an environment <> current environment
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
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## retrieves and returnsthe inverse from the cache- which is non null
    return(m)
  }
  else 
  {data <- x$get()
  m <- solve(data, ...)
  ## otherwise calculate the inverse of x and return the value from the cache- which is non null
  x$setinverse(m)
  m
  }
}

