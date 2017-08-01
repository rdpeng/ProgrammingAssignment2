makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setmean = setinverse,
       getmean = getinverse)
}
  

cacheSolve <- function(x, ...) 
{
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
