## functions to create 'special' matrix list that stores, returns and solves the inverse of a given matirx

## make a cacheable matirx
##return a list of functions to get and set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## function to set parent object 'x' and matrix y and reset inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##function to return matrix 'x'
  get <- function() x
  ## function to set inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  ## function to return cached inverse matrix
  getinverse <- function() inv
  ## returns list of functions
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## check to see if list "x" has a cahced inverse matrix and return it
cacheSolve <- function(x, ...) {
  ## returns cached inverse matrix
  inv <- x$getinverse()
  ## returns inverse if cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## returns matric data
  data <- x$get()
  ## finds inverse of matrix
  inv <- solve(data)
  ## sets inverse of matirx
  x$setinverse(inv)
  ## returns inverse of matrix
  inv
}
