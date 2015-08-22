makeCacheMatrix <- function(x = matrix()) { ## input x for the function needs to be an invertible matrix
  m <- NULL        ## set the value of the inverse matrix (m) to null everytime when the function is run in a new parent environment (not by just calling get or getinverse, one of the child environments)
  set <- function(y) { ## set function to set a new value for the input matrix
    x <<- y            
    m <<- NULL ## overwrite the old value for the inverse matrix
  }
  get <- function() x ## function to get the current value of the input matrix
  setinverse <- function(solve) m <<- solve ## set the new value for inverse matrix, by using solve function
  getinverse <- function() m ## function to get the current value of the inverse matrix
  list(set = set, get = get, ## special list vector, containing all the child environments that can be called as functions
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## call the getinverse from makeCacheMatrix to set the current value of the inverse matrix m
  if(!is.null(m)) { ## if the current value is not "null", the cached value can be used
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## if m is "null", a new value for m is calculated
  m <- solve(data, ...)
  x$setinverse(m) ## child environment setinverse is used to set the new value for inverse matrix for m
  m 
}