## Put comments here that give an overall description of what your
## functions do
##	The functions makeCacheMatrix and cacheSolve can be used to 
##	solve a linear matrix equation to obtain the inverse of a matrix.
##	The neat trick is to check if the inverse of the matrix exists (cache)
##	before computing it in order to save time.


## Write a short comment describing this function
	## makeCacheMatrix puts its matrix argument (x) in a special 
	## list whose 4 function elements are:
	## 	$set			to put the matrix into the cache
	## 	$get			to get the matrix from x
	## 	$setInverse	to put the inverse matrix into the cache
	## 	$getInverse	to read the inverse matrix
	
	## The argument x is a square numeric matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) cacheinv <<- inv
	getInverse <- function() cacheinv
	list (get = get, set = set, 
	setInverse = setInverse, getInverse= getInverse)


}

## Write a short comment describing this function
	## 	cacheSolve solves the linear equation to get the inverse of a matrix
	##	The argument x is a list with four elements prepared by the function 
	##	makeCacheMatrix


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv  
}
