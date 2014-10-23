## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following functions are used to cache the inverse of a matrix.

## 1. set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## 2. get the value of the matrix

    get <- function() x
    
## 3. set the value of the inverse

    setinverse <- function(inverse) inv <<- inverse
## 4. get the value of the inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function
## The following function calculates the mean of the special "matrix" created with the above function. 
##However, it first checks to see if the mean has already been calculated. If so, it gets the mean from 
##the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of 
##the mean in the cache via the setmean function.

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
}
## Sample run:
x<-matrix(1:10;2;3)
m<-makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)
