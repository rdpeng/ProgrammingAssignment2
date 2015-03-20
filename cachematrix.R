## 20 MAR 2015 LuiRC

##
## This file contains two functions that use the <<- operator to 
## assign a value to an object in an environment that is different 
## from the current environment. The first fucntion creates a special 
## matrix object and the second function is used to access the inverse
## matrix by either generating it or accessing a previously cached
## version if it has already been calcualted.
## WARNING:
## As this is a programming exercise it assumed that all provided 
## matrixes are invertible, and there is no check performed for this.  
##
## The code below is based on the "vector" example given at 
## https://class.coursera.org/rprog-012/human_grading/view/courses/973493/assessments/3/submissions
## last accessed on 20 MAR 2015

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing separate functions/methods to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse 
## get the value of the inverse 
##

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse 
	getinverse <- function() m
	list(
		set = set, 
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
		)
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the array and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
