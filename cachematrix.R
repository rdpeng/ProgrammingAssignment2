## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	y <- NULL
  set <- function(z){
    x <<- z
    y <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) y <<- inverse
  getInverse <- function() y
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  y <- x$getInverse()
  if(!is.null(y)){
    message("getting cached data")
    return(y)
  }
  mat <- x$get()
  y <- solve(mat,...)
  x$setInverse(y)
  y
}
