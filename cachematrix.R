##
## These functions cache the inverse of a matrix rather than compute it repeatedly
## 


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Create the setters and getters
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  # Make the Setters and Getters available to the outside
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                 ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {                   ## Check to see if we created the inverse already
    message("getting cached data")    ## Yes we did, return it.
    return(m)
  }
  data <- x$get()                     ## No we did not, Get the matrix
  m <- solve(data, ...)               ## Compute the inverse
  x$setinverse(m)                     ## Cache the inverse and return it.
  m
}


