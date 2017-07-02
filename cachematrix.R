## makeCacheMatrix will create a memory cache for the execution of the
## Solve(function on the matrix input when assigned to an object.
## cacheSolve will take as input the above object and calculate the inverse
## of the matrix unless the matrix has not changed in whcih then it will
## recover the cached inverse from makeCacheMatrix

## Creates a list of function('setmatrix', 'getmatrix', 'setinverse',
## and 'getinverse') when assigned to an object

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	setmatrix <- function(y){
		x <<- y
		i <<- NULL
}

	getmatrix <- function()x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function()i
	list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)

## Solves the matrix input and returns its inverse, will return the cached
## solution if any has been stored by makeCacheMatrix and matrix is unchanged

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if (!is.null(i)){
		message('retrieving cached data')
		return(i)
	}
	data <- x$getmatrix()
	i <- solve(data, ...)
	x$setinverse(i)
	i
        ## Return a matrix that is the inverse of 'x'
}
