## Those are functions able to create a special matrix object that can be inverted
## When the inverse is requiered it searchs for the existance of the inverse stored on cache
## if the matrix has not changed. Otherwise, the inverse is recomputed.

## Try this example to know how it works:
## A <- makeCacheMatrix(matrix(1:4,2,2))
## I <- cacheSolve(A)



## This function is able to create a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
#Defines x as a matrix and saves it to cache

        MyInverse <- NULL # Creates MyInverse which is going to have a value later
        
        Set <- function(y) {
                x <<- y
                MyInverse <<- NULL
        }
        
        Get <- function() x
        SetInverse <- function(solve) MyInverse <<- solve
        GetInverse <- function() MyInverse
        list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse) # Creates a list containing 
        # the matrix and its inverse (solve function)
        
}



## This function computes the inverse of the special matrix returned by the function makeCacheMatrix above.
## It evaluates whether the matrix has not changed, then the function cacheSolve retrieves the inverse from
## the cache.


cacheSolve <- function(x, ...) {

        MyInverse <- x$GetInverse()  # Gets the inverse from the matrix 
        if(!is.null(MyInverse)) {  # If the matrix has not changed and the inverse has already been calculated
                message("Getting cached data")  # it retrieves the inverse from the cache
                return(MyInverse)
        }
                
        MyData <- x$Get()  # If it is a new matrix, it calculates the new inverse
        MyInverse <- solve(MyData, ...)
        x$SetInverse(MyInverse)
        MyInverse
}
