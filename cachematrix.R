## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 
  I <- NULL
  get <- function() x
  set <- function(y = matrix()) {
    x <<- y
    I <<- NULL
    return(x)
  }
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 I <- x$getInverse()
  if(!is.null(I))
  {
    message("getting cached inverse")
    x$getInverse()
  }
  
  data <- x$get()
  Inverse <- solve(data)
  x$setInverse(Inverse)
  Inverse
        ## Return a matrix that is the inverse of 'x'
}
