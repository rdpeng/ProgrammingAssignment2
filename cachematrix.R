## This is a function able to cache the inverse of a matrix
## in order to not repeating the calculation

 

## This function is able to create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
#Defines x as a matrix and saves it to cache

        MyInverse <- NULL
        
        Set <- function(y) {
                x <<- y
                MyInverse <<- NULL
        }
        
        Get <- function() x
        SetInverse <- function(solve) MyInverse <<- solve
        GetInverse <- function() MyInverse
        list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
        
}



## This function computes the inverse of the special matrix returned by the function makeCacheMatrix above.
## It evaluates the matrix and if it has not changed then the function cacheSolve retrieves the inverse from
## the cache.


cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'

        MyInverse <- x$GetInverse()
        if(!is.null(MyInverse)) {
                message("Getting cached data")
                return(MyInverse)
        }
                
        MyData <- x$Get()
        MyInverse <- solve(MyData, ...)
        x$SetInverse(MyInverse)
        MyInverse
}
