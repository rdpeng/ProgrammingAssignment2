## the functions will store data (cache) of inverse of a matrix if calculated before
 

## makeCacheMatrix will get, set, getInverse ,setInverse of input matrix x
##inverse of x is i

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will look for inverse of matrix x if calculated before
## it will return it
## else it will get x and calculate its inverse by "solve()"
## then it will set the value of new inverse

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
