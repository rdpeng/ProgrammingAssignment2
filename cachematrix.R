## Submission for Assignment 2 of Coursera-R
##
## I have written 2 functions:
## a. makeCacheMatrix
## b. cacheSolve

## 1. The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## 2. I want to acknowledge that my this function definition is extended from the example given in 
## course assignment on caching means of vectors, by Dr. Peng. I have extended it to a matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
					x <<- y
					m <<- NULL
    		}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}



## 1. The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## 2. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## function should retrieve the inverse from the cache.
## 3. I want to acknowledge that my this function definition is extended from the example given in the
## course assignment on caching means of vectors, by Dr. Peng. I have extended it to a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data.")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}
