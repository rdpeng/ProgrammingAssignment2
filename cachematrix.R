## makeCacheMatrix is an object with 3 functions: get, setinv and getinv
## which are called by another function named cacheSolve. The object cacheSolve
## is a function which calls the list of functions in makeCacheMatrix. 

## This "object list" has the main objective of storing a matrix and its
## inverse. The list of functions are accessed by another function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function calculates the inverse of the matrix defined in the object
## makeCacheMatrix. If it's already stored it returns a message and retrieves the 
## value; otherwise, it makes the calculation and stores the value in the 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
