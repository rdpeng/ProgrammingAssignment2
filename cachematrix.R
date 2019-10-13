## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
  
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix <<- inverse
  getinverse <- function() matrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  matrix <- x$getinverse()
  if (!is.null(matrix)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setinverse(matrix)
  matrix
}