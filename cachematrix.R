

## This function creates a special matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) { #x is the input matrix, m is the matrix inverse of x
  m <- NULL
  
  set <- function(y) {
    x <<- y 
    m <<- NULL 
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve #assigning m the inverse of x
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# this function takes x as an input argument from function makecachematrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)   # if no value for m, then return the cached m value
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setinverse(m)
  
  m
}
