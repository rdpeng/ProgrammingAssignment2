## Final version of the Programming Assignment 2 script (16/12/2014)
## This script defines two functions:
## 1.makeCacheMatrix which produces a list of four function to get/set the inverse of a matrix in cache
## 	$set(y) : sets a new matrix y (via x <<-y) and clears the cache (via m <<-NULL)
## 	$get()  : returns the matrix stored in the function
## 	$setinverse(inv) : sets the cache to inv
## 	$getinverse() : returns the cached inverse (or NULL if not cached)

## 2.cacheSolve to calculate the inverse or to get it from cache

makeCacheMatrix <- function(x = matrix()) {
	
	# sets the inverse to NULL
	m <- NULL
	# sets a new matrix y and clears the cache
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	# get the matrix
	get <- function() x
	# set the inverse of the matrix
	setinverse <- function(inv) m <<- inv
	# get the inverse of the matrix
	getinverse <- function() m
	
	# return the list of functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

	# check of there is an inverse in cache	
	m <- x$getinverse()

	# if so, then get it from cache
	if(!is.null(m)) {
		message("getting cached data")
		# return m and exit function
		return(m)
	}
	# get the matrix
	data <- x$get()
	
	# solve the inverse via the solve function
	m <- solve(data, ...)
	
	# set the inverse 
	x$setinverse(m)
	
	# return m
	m
}

## code to test the function

## source("cacheMatrix.R")
## x <- sample(20,16)
## dim(x) <- c(4,4)
## m <- makeCacheMatrix(x)
##
## first run of the cacheSolve (so must cache the inverse)
## cacheSolve(m) 
## 
## second run of cacheSolve (will get the inverse from cache)
## cacheSolve(m)
##
## make a new matrix
## x <- sample(20,16)
## dim(x) <- c(4,4)
## m$set(x)
## cacheSolve(m)
