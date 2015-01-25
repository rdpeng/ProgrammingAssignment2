# makeCachemetrix creates a matrix that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Trial Run
x <- matrix(c(2,4,3,1,9,6,5,8,7), nrow=3, ncol=3)
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)