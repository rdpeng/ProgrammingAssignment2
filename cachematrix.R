## Put comments here that give an overall description of what your
## functions do

## The purpose of the two functions below is to cache the computation of the inverse of a matrix.
## The makeCacheMatrix creates a specila matrix object that can cache its inverse.
## cacheSolve is a function that computes the inverse returned by makeCacheMatrix. If the inverse
## has already been calculated, then cacheSolve retrieves the inverse from the cache.




## Write a short comment describing this function
## This function stores 4 functions: set, get, setinv and getinv.
## 'get' is a function that returns the matrix that is stored in the main function makeCacheMatrix
## set is a functtion that changes the matrix stored in the main function makeCacheMatrix
## 'setinv' doesn't calculate the inverse of the matrix. It stores the vlaue of the input in a variable 'inv' into the
## main function makeCacheMatrix.
## 'getinv' simply returns that inverse that was set by 'setinv'.


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The first thing cacheSolve does is verifies the value 'inv' stored previously with getinv
## if 'inv' is not NULL, it simply returns a message and the value 'inv'.
## IF this is the case, then "return(inv)" ends the function.
## IF this is not the case, then 'data' gets the matrix stored with makeCacheMatrix and 'inv' calculates the
## inverse of that matrix and 'x$setinv(inv)' stores it in the object generated assigned with makeCacheMatrix.


cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv			## Return a matrix that is the inverse of 'x'			
}

