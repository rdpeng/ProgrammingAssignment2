## function makeCacheMatrix will cache inverse of a Matrix. 
## function cacheSolve returns Inverse of a Matrix. If the inverse is already cached
## function cacheSolve will return the cached inverse matrix.

## makeCacheMatrix function takes a matrix x as input, calculates its inverse function solve
## and cache the resulting inverse metrix in s. creates a list to store all the values of the parent environment.

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
}


## cacheSolve function checks if the the Matrix is cached, if it is is cache, it returns matrix from the cache else 
## it calculates the inverse and returns the inverse matrix

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
