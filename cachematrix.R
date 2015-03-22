## Both these functions together used to cache the inverse of a matrix

## This function does the following
## Sets the value of the matrix
## Gets the value of the matrix
## Sets the value of inverse of the matrix
## Gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
  get <- function() x
  setinverse <- function(inverse)
  inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of a matrix if there the inverse is not yet computed.
## If the inverse has already been computed, this function will return those values directly instead of computing them again.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
## > x = rbind(c(1/2, -2/7), c(1/3, 2))
## > m = makeCacheMatrix(x)
## > m$get()
##          [,1]       [,2]
## [1,] 0.5000000 -0.2857143
## [2,] 0.3333333  2.0000000
## > cacheSolve(m)
##           [,1]      [,2]
## [1,]  1.8260870 0.2608696
## [2,] -0.3043478 0.4565217
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,]  1.8260870 0.2608696
## [2,] -0.3043478 0.4565217

}
