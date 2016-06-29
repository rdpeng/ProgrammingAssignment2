## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # initiation
  inv <- NULL
  
  # set the value of the matrix, and remove the old inverse
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinv <- function(solution) inv <<- solution
  
  # get the value of the inverse
  getinv <- function() inv
  
  # create a list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # get the inverse stored in the cache
  inv <- x$getinv()
  
  # use the existing inverse if there is one
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # set the inverse if it hasn't been computed
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)

  ## Return a matrix that is the inverse of 'x'
  inv
}
