## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        f <- NULL
  set <- function(y)
    {
    x <<- y
    f <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) f <<- inverse
  getInverse <- function() f 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## following code Return a matrix that is the inverse of 'x'   
        f <- x$getInverse()
  if(!is.null(f))
    {
    message("getting cached data")
    return(f)
  }
  mat <- x$get()
  f <- solve(mat,...)
  x$setInverse(f)
  f    
}
