## Functions written for partial fulfillment of Coursera R Programming course and to complete Week 3 assignment.
## @author: Arnav Jain (arnav58). @dated: 28 May, 2017.

## These functions maintain a cache of the inverse of matrix once it is calculated, to avoid the computational overhead of the same when user needs the inverse again.

## Function sets, maintains and returns cached values for inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
	## initializing inverse matrix
	inverse <- NULL

	## Function to set the value of cacheMatrix. Takes simple matrix() as input.
	set <- function(y) {
		## Setting values to parent environment to enable caching.
		x <<- y
		inverse <<- NULL
	}

	## Function to get the cacheMatrix created with set method.
	get <- function() {
		## Returning cacheMatrix
		x
	}

	## Function to set the inverse value of matrix once calculated.
	setinverse <- function(inv) {
		Updating the value of inverse object initialized with set method.
		inverse <<- inv
	}

	## Function to get the cached value of the inverse.
	getinverse <- function() {
		## Returning inverse value.
		inverse
	}
	
	## To enable referencing of functions with $.
	list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function to get inverse of matrix. Returns cached value, if any. Else, calculates the inverse, returns the value and caches the same.
cacheSolve <- function(x, ...) {
	## Get cached value of inverse.
	inverse <- x$getinverse()
	
	## Checking if the cache value retrieved is null.
	if(!is.null(inverse)) {
		## Returning the chached value when not null.
		return(inverse)
	}

	## Calculating the inverse if cached value is null.
	mat <- x$get()
	inverse <- solve(mat)

	## Setting the calculated value to cache.
	x$setinverse(inverse)
	
	## Returning calculated inverse matrix.
	inverse
}
