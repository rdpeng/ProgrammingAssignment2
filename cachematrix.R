## Put comments here that give an overall description of what your
## functions do

## This function is to set the value of the matrix and get the value of the
## matrix; set the value of the inverse of the matrix and to get the 
## the value of the inverse.

 makeCacheMatrix <- function(x = matrix()) {
  +       m <- NULL
  +       set <- function(y) {
    +             x <<- y
    +             m <<- NULL
    +     }
  +       get <- function() x
  +       setinverse <- function(solve) m <<- solve
  +       getinverse <- function() m
  +       list(set = set, get = get,
               +            setinverse = setinverse,
               +            getinverse = getinverse)
  + }




## The following function calculates the inverse of the matrix
## It first checks whether the inverse has been calculated. 
## If it is calculated, it will get the inverse from the cache and skip
## the computation. If not, it will calculate and set the value of the 
## the inverse in the cache through the setinverse function.
cacheSolve <- function(x, ...) {
  +       m <- x$getinverse()
  +       if(!is.null(m)) {
    +             message("getting cached data")
    +             return(m)
    +     }
  +       data <- x$get()
  +       m <- solve(data, ...)
  +       x$setinverse(m)
  +       m
  }
## The following is to use a matrix to get solve of inverse
a<-diag(9,4)
CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)


