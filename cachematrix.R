## This two functions are used to cache the inverse of a matrix to save
## computation and memory of computer.

## The first function "makeCacheMatrix" creates a special "matrix", which is 
## list of functions to set the value of matrix; get value of matrix; set the
## value of inverse; get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculate the inverse matrix of the special "matrix" create
## by above function. It first check if the inverse matrix has been calculated
## and saved in the cache via getinverse(), if not, it calculate the inverse 
## matrix by solve() function and save it in cache by setinverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
