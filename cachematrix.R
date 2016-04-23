## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function below: A "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inverse
          list(set = set, 
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Write a short comment describing this function
## Function computes the inverse of the "matrix" returned by makeCacheMatrix 

cachesolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
            }
            mat <- x$get()
            inv <- solve(mat, ...)
            x$setinverse(inv)
            inv
}
## Return a matrix that is the inverse of 'x'