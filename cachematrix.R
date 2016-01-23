makeCacheMatrix <- function(X = matrix()) {
    ## this function can cache the inverse of a matrix
    ##    input:  X, a square invertible matrix
    ##    output: a list of functions to:
    ##      - set the value of X
    ##      - get the value of X
    ##      - set the value of the inverse of X
    ##      - get the value of the inverse of X
    ##  this list will be the input to cacheSolve()
  
  inv <- NULL
  set <- function(Y) {
    X <<- Y
    inv <<- NULL
  }
  get <- function() X
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ##  this function computes the inverse of the matrix returned by makeCacheMatrix
    ##    input:    a special matrix object created by makeCacheMatrix
    ##    output:   the inverse of the matrix returned by makeCacheMatrix
    ##    note:     if the inverse has already been calculated (and the matrix hasn't changed),
    ##                cacheSolve should retrieve the inverse from the cache
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}

testAssignment <- function(X) {
  ## input: X a square invertible matrix
  
  my.matrix <- makeCacheMatrix(X)
  print(system.time(my.inv <- cacheSolve(my.matrix)))
  print(system.time(my.inv <- cacheSolve(my.matrix)))
  print(system.time(my.inv <- cacheSolve(my.matrix)))
  ## head(X %*% my.inv)
}

set.seed(7386420)
rmat <- matrix(rnorm(1000000),1000,1000)

testAssignment(rmat)
