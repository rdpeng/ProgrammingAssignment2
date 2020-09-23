## The makeCacheMatrix function will create a special matrix object which will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) inv <<- solve
 getinverse <- function () inv
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function will calculate the inverse matrix which is the result of the function above. 
## It will take the output from the cache if the inverse was already calculated.

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
