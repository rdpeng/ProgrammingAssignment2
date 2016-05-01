## Put comments here that give an overall description of what your
## functions do

#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
#(there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

#set the value of the Matrix
#get the value of the Matrix
#set the Inverse of the Matrix
#get the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
	     setinverse=setinverse, 
		 getinverse=getinverse)

}


## Write a short comment describing this function

## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## How to use this function
#let's input a matrix as
#x = rbind(c(1, .5), c(.5, 1))
#get the input value
#y<-makeCacheMatrix(x)
#y$get()

#get the inverse of the matrix for the 1st time
#cacheSolve(y)

#get the inverse of the matrix for the 2nd time onwards, it should display as "getting from cache"
#cacheSolve(y)
