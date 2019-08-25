## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(A = matrix()) {
    i <- NULL
  set <- function(y) {
          A <<- y
          i <<- NULL
  }
  get <- function() A
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

## Write a short comment describing this function

cacheSolve <- function(A) {
  i <- A$getinverse()
  if (!is.null(i)) {
          message("getting cached dat")
          return(i)
  }
  dat <- A$get()
  i <- solve(dat)
  A$setinverse(i)
  i
}
