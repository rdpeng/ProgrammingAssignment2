## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## This assignment writes a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
  
a <- diag(2, 7)
a

## [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]    2    0    0    0    0    0    0
## [2,]    0    2    0    0    0    0    0
## [3,]    0    0    2    0    0    0    0
## [4,]    0    0    0    2    0    0    0
## [5,]    0    0    0    0    2    0    0
## [6,]    0    0    0    0    0    2    0
## [7,]    0    0    0    0    0    0    2
  
CacheMatrix <- makeCacheMatrix(a)
cacheSolve(CacheMatrix)

## [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]  0.5  0.0  0.0  0.0  0.0  0.0  0.0
## [2,]  0.0  0.5  0.0  0.0  0.0  0.0  0.0
## [3,]  0.0  0.0  0.5  0.0  0.0  0.0  0.0
## [4,]  0.0  0.0  0.0  0.5  0.0  0.0  0.0
## [5,]  0.0  0.0  0.0  0.0  0.5  0.0  0.0
## [6,]  0.0  0.0  0.0  0.0  0.0  0.5  0.0
## [7,]  0.0  0.0  0.0  0.0  0.0  0.0  0.5
  
CacheSolve(CacheMatrix)

## getting cached data
## [,1] [,2] [,3] [,4] [,5] [,6] [,7]
## [1,]  0.5  0.0  0.0  0.0  0.0  0.0  0.0
## [2,]  0.0  0.5  0.0  0.0  0.0  0.0  0.0
## [3,]  0.0  0.0  0.5  0.0  0.0  0.0  0.0
## [4,]  0.0  0.0  0.0  0.5  0.0  0.0  0.0
## [5,]  0.0  0.0  0.0  0.0  0.5  0.0  0.0
## [6,]  0.0  0.0  0.0  0.0  0.0  0.5  0.0
## [7,]  0.0  0.0  0.0  0.0  0.0  0.0  0.5


  