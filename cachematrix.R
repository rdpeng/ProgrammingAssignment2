## This program will calculate the inverse of the cached matrix.


## The function called makeCacheMatrix will make a cached matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve will get a cached matrix from makeCacheMatrix and compute the inverse matrix.

cacheSolve <- function(x, ...) {
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

