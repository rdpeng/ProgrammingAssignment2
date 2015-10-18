## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minverse <<- inverse 
  getinverse <- function() minverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("getting cached matrix inverse")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
