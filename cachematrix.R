## makeCacheMatrix is a special function that creates matrix by an object
## in an environment that is different from current one. 
## cacheSolve is the function that calculates inverse matrix of the cacheMatrix

## makeCacheMatrix is the function that creates cache of matrix
## and also list to get and set matrix and get and set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve calculates inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
