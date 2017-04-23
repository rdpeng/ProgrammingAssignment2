## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # creates a null matrix that stores cache value
  cache <- NULL
  # gets and sets the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() {
    x
  }
  
  setInverse <- function(solve){
    cache <<- solve 
  } 
  # It returns inverse of the matrix
  getInverse <- function() {
    cache
  }
  list(set=set, get=get, setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
# returns the inverse of the matrix
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # returns the inverse if it is there in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # calculates the inverse of matix
  m <- solve(data)
  #caches the inverse 
  x$setInverse(m)
  # returns the inverse 
  m
  
  
}
