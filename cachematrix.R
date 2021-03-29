
## functions do

## makecacheMatrix creates a matrix objet in order to invert it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## I set inv to NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}



## cacheSolve sets a pathway for the matrix, sets a default message and solves the inversion

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
## Return a matrix that is the inverse of 'x'



