## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  #set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  
  
  #get the matrix
  get <- function() x
  
  # set the inverse of the matrix here and calculating the inverse here
  setInverse <- function() inv <<- solve(x) 
  # get the inverse here
  getInverse <- function() inv
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getInverse()
  
  # if the inverse is already been calculated then We get the inverse from cache
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # else the inverse is calculated
  mat <- x$get()
  inv <- solve(mat, ...)
  
  #Setting the  value of inverse in setInverse
  x$setInverse(inv)
  inv
  
}
