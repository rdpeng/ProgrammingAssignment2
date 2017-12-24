## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function above is to compute the inverse of the special "matrix" returned by makeCacheMatrix above.

makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- funtion() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

        ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("get from cached data")
    return(inv)
  }
  
  mat.data <- x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  (inv) 
}