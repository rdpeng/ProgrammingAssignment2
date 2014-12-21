## Matrix inversion is usually a costly computation and their may be some 
## benefit, especially when the data size is big, to caching the inverse of 
## a matrix rather than compute it repeatedly.


#################### makeCacheMatrix #########################################
## This function creates a special "matrix" object that can cache its inverse.
## This function  is really a list containing a function to  
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. setInverse: set the value of Inverse of the matrix
## 4. getInverse: get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	# initialize inverse value to null
	vInv <- NULL

	# Set matrix: 
	set <- function(y) {
			x <<- y
			vInv <<- NULL
		}
	
	# Get matrix: 
	get <- function() x

	# Set inverse
	setinverse <- function(inverse) vInv <<- inverse
	
	# Get inverse
	getinverse <- function() vInv
	list(Set=set, Get=get, SetInverse=setinverse, GetInverse=getinverse)	

}

#################### cacheSolve #########################################
## The following function calculates the inverse of the matrix computed and created with 
## the "makeCacheMatrix" function. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse of the matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the SetInverse() function.
## This function assumes that matrix is invertible and hence no error handling has been implemented.

cacheSolve <- function(x, ...) {
		
		#Get the inverse
		vInv <- x$GetInverse()
		
		#If no cached data then calculate the inverse, cache the data and return the inverse
		if(is.null(vInv)) {
				vData <- x$Get()
				vInv <- solve(vData)
				x$SetInverse(vInv)
				return(vInv)
		}
		
		message("Retrieving from cached data...")
		return(vInv)
}

## Test case
## > A = matrix( c(2,2,3,2), nrow=2, ncol=2)
## > m = makeCacheMatrix(A)
## > m$Get()
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(m)
## Retrieving from cached data...
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
