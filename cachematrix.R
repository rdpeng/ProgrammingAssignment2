## Put comments here that give an overall description of what your
## functions do

# getting started

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    print("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- inverse(data, ...)
  x$setinverse(s)
  s
}
