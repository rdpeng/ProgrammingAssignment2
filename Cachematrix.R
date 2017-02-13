makeCacheMatrix <- function(x = matrix()) {         
  o <- NULL
  set <- function(y) {
    x <<- y      
    o <<- NULL   
  }
  get <- function() x  
  setinverse <- function(solve)  o <<- solve
  getinverse <- function() o
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve<- function(x, ...) {
  o <- x$getinverse()
  if(!is.null(o)) {
    message("getting cached data")
    return(o)
  }
  data <- x$get()
  o <- solve(data, ...)
  x$setinverse(o)
  o
}
