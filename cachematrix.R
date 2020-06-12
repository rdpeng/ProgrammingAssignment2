## This function produces a special matrix "x" and sotres its inverse in cache and if the inverse is
## not already in the cache then it calculates the inverse of the matrix and stores it in the cache.


## makeCacheMatrix is function that produces a sepcial matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is function that that inputs a matrix x and solve it to get its inverse. If the inverse
## of that matrix is already calculated then it gets the inverse from cache and if not then it calculates
## the inverse and stores the inverse of that matrix in cache.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}


