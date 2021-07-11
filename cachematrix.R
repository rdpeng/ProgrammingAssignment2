## In this hands-on assignment, will tackle how to create such certain variable
## The variable or its function will be named in for its references further on making the script process
## The function "makeCacheMatrix" is created to start the function of the script. It creates the values of the numeric and solve the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
    
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## The "cacheSolve" argument uses to calculate tsourche given "makeCacheMatrix" statement and inversed the numerics.
## Now that the argument is processing, it will take the calculated inverse, example 1 is to -1 and so on..

cacheSolve <- function(x, ...) {
  ## The pattern is the X inverse
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv}