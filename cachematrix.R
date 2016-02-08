## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

## Solution
## .....................................
##makeCacheMatrix and cacheSolve are the two functions used to
##return the inverse of a matrix from cache
##makeCacheMatrix shall create a list with a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
##cacheSolve first checks if the inverse of the matrix has been calculated.
## If it has,it skips the calculation and gets the result from cache.
## However, if not, it calculates the inverse and sets the value in the cache
## by the setinverse function
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
##testrun
## Forming a 3x3 matrix from 3 vectors
## x=rbind(c(1,2,3),c(0,1,4),c(5,6,0))
## m = makeCacheMatrix(x)
## m$get()
##      [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    1    4
##[3,]    5    6    0
## In the first run, cache will not be used
## cacheSolve(m)
##      [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1
## For the same calculation the second time, cache will be used
## cacheSolve(m)
##getting cached inverse.
##      [,1] [,2] [,3]
##[1,]  -24   18    5
##[2,]   20  -15   -4
##[3,]   -5    4    1







