## function makeCacheMatrix will cache inverse of a Matrix. 
## function cacheSolve returns Inverse of a Matrix. If the inverse is already cached
## function cacheSolve will return the cached inverse matrix.

## makeCacheMatrix function takes a matrix x as input, calculates its inverse function solve
## and cache the resulting inverse metrix in s. creates a list containing function to 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse matrix
##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

s <- NULL
set <- function (y){
	x <<- y
	s <<- NULL
	}
get <- function () x
setsolve <- function( solve) s <<- solve
getsolve <- function() s
list ( set = set, get = get, setsolve = setsolve, getsolve = getsolve)
 
}


## The following cacheSolve function calculates the inverse of the matrix created by above function and returns the inverse matrix 
## In the process. it first checks to see if the inverse has already been calculated. If so, it gets the inverse matrix 
## from the cache and skips the computation. Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix
## in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
         
s <- x$getsolve()
if(!is.null(s)){
	message(" getting from cached data")
	return(s)
	}
data = x$get()
s <- solve(data,...)
x$setsolve(s)
s
}
