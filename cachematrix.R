## Put comments here that give an overall description of what your
## functions do

## In fact functions nead only to change computation function from mean to solve, rest works at the same way, with setters and getters. 
## Small difference is also an function argument, that is not a vector, but matrix, as per assignement, there is not need to check whenever
## matris is invertable, as we assume it is. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(whatever) m <<- whatever
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This is function, that calculates inverse (solve) if not yet inverted erlier. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data of inveresed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
