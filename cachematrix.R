## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# the function set the matrix and the inverse of the matrix as null, than calculate the inverse of the matrix with setsolve, 
# and provide the list of functions with getsolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}



## Write a short comment describing this function

# this function first check if the inverse of the matrix has been already calculated, and if this is not present,
#it calculates the inverse 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("calculate the inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}


