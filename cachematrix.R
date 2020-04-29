
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) z <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  z <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(z)
  z      
}
## Return a matrix that is the inverse of 'x'