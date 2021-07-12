## In this hands-on assignment which is "Lexical Scoping", it tackles about how to create such certain variables
## The variable or its function will be named in for its references further on making the script process or you can set it for your own references
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

## Now making the code structure for the "cacheSolve" function..
## The "cacheSolve" function uses to calculate the given "makeCacheMatrix" statement and inverse the numeric.
## Now that the argument is processing, it will take the calculated inverse, example 1 is to -1 and so on..

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'"
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv}