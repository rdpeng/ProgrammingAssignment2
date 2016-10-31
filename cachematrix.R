## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  z <<- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setzerse <- function(zerse) z <<- zerse
  getzerse <- function() z
  list(set=set,get=get,setzerse=setzerse,getzerse=getzerse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  z <- x$getzerse()
  if(!is.null(z)) {
    message("getting cached data.")
    return(z)
  }
  data <- x$get()
  z <- solve(data)
  x$setzerse(z)
  z
}
