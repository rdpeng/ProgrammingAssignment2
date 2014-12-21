## Store a matrix and the inversion

## makeCacheMatrix function has an argument, which is a matrix 
## and returns a kind of class object.
## When you call the function without argument, it returns 
## empty class object.
## 
## USAGE: x <- makeCacheMatrix(matrix_object)
##
## The list of methods (x is the object returned from the function)
## x$set: Set new matrix
## x$get: Get the stored matrix
## x$setinv: Set an inverse matrix
## x$getinv: Get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}

## The function cacheSolve has an argument, which is the object
## returned from makeCacheMatrix function.
## When the inverse matrix has not been assigned, calcalate the
## invese matrix and set it to the object.
## When the inverse matrix is already assigned, funtion returns
## cached matrix
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

