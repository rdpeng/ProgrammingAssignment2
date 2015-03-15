## R Programming Assignment 2: Lexical Scoping

## makeCacheMatrix
## makeCacheMatrix creates a special matrix object that can cache
## its inverse.
## This is a list containing a function designed to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {		##creates object x
	cacheinv <- NULL				## defines the cache cacheinv
	setMatrix <- function(new) {
                x <<- new
                cacheinv <<- NULL
        }

	## 'Get' and 'set' functions for the inverse

	 getMatrix <- function() x

        cacheInverse <- function(inverse) cacheinv <<- inverse

        getInverse <- function() cacheinv

        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## cacheSolve
## cachesolve is a function designed to calculate the inverse of 
## the special matrix created with makeCacheMatrix above.
## This function checks to see if the inverse has already been calculated.
## If it has, it gets the inverse from the cache, if it hasn't, the function
## calculates it.

cacheSolve <- function(x, ...) {
		## Returns a matrix for the inverse of x

        inversesolve <- x$getInverse()
        if(!is.null(inversesolve)) {
                message("getting cached data")
                return(inversesolve)
        }
        data <- x$getMatrix()
        inversesolve <- solve(data)			##Calculates the inverse
        x$cacheInverse(inversesolve)
        inversesolve
}
