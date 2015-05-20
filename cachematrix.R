makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  mat <- matrix(c(set,get, setinverse, getinverse), ncol = 4)
  colnames(mat) <- c(set,get, setinverse, getinverse)
  mat
}

cachesolve <- function(x, ...) 
{
  inv <- x[,"getinverse"]()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x[,"get"]()
  inv <- solve(data, ...)
  x[,"setinverse"](inv)
  inv
}
