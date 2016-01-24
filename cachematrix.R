## when used together, makeCacheMatrix and cacheSolve can calculate or cache
## values for a matrix and that matrix's inverse.


## makeCacheMatrix is a list of functions that can store and return 
## values for one matrix and that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve will return the inverse for matrix x if cacheSolve is applied makeCacheMatrix(m).
## If an inverse for matrix m has not been set, cacheSolve will calculate, set, and print the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinv(inv)
  inv
}

#run this to test functions work properly when no inverse is cached:
m <- matrix(c(2,0,0,0,1,0,0,0,1), 3, 3)
makeCacheMatrix(m) -> v
cacheSolve(v)


#run this to test setting and printing inverse:
v$setinv(matrix(c(20,0,0,0,1,0,0,0,1), 3, 3))
v$getinv()
cacheSolve(v)

#end