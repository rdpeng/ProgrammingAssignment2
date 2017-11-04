## As part of Week 3 of the R Programming part of the Coursera Data Science course
## I wrote a pair of functions that cache the inverse of a matrix. 
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

## This fist functoin creates a "matrix" that caches its inverse. It consists of the following list:
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
                getinverse <-function() inv
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}

## This second function computes the inverse of the "matrix" returned by makeCacheMatrix.
## The cacheSove will retrieve the inverse from the cache.
## It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
