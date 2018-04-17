 ## Caching the Inverse of a Matrix
 
 ## Matrix inversion is usually a costly computation and 
 ## there may be some benefit to caching the inverse of a matrix
 ## rather than compute it repeatedly
 
 makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) inv <<- inverse
         getinverse <- function() inv
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }
 
 ## The following function returns the inverse of the matrix. It first checks if
 ## the inverse has already been computed. If so, it gets the result and skips the
 ## computation. If not, it computes the inverse, sets the value in the cache via
 ## setinverse function.
   
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
 
 ##test the function
 testdata=matrix(1:4,2,2,byrow=TRUE)
 test=makeCacheMatrix(testdata)
 testinverse=cacheSolve(test)
 testdata
     [,1] [,2]
[1,]    1    2
[2,]    3    4
 testinverse
     [,1] [,2]
[1,] -2.0  1.0
[2,]  1.5 -0.5
 
 testinverse==solve(testdata)
     [,1] [,2]
[1,] TRUE TRUE
[2,] TRUE TRUE
 
