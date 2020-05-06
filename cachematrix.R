## *makeCacheMatrix*: This function creates a special "matrix" object that can cache its inverse
## *cacheSolve*: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverese <- function() inv
list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setinv(i)
  i
}
