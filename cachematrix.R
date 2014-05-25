## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix defines a matrix as a list of functions:
# 1 set can be used to overwrite the 'x' data in the 'makeCacheMatrix'-environment that is called upon by 'cacheSolve'
# 2 get is used to retrieve the original data in the 'makeCacheMatrix'-environment for computing a new inverse
# 3 setinv is used to store the new inverse from the 'cacheSolve'-environment in the 'makeCacheMatrix'-environment
# 4 getinv is used to load the stored inverse from the 'makeCacheMatrix'-environment into the 'cacheSolve'-environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve first checks whether an inverse is already stored in the 'makeCacheMatrix'-environment. 
# If so, it returns this inverse with a message.
# If not, it retrieves the matrix from the 'makeCacheMatrix'-environment, solves it, saves it, and returns it.

cacheSolve <- function(x, ...) {  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}