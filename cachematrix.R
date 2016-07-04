## makeCacheMatrix is a list containing function to
## 1. set : set the value of matrix
## 2. get : get the value of matrix
## 3. setinverse : set the value of inverse matrix
## 4. getinverse : get the value of inverse matrix


## This function is used to hold the matrix and the its inverse. 
## The inverse is not calculated within this function. 
## It is expected to be calculated outside and stored in this.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function () x

	setinverse <- function(invmatrix) inv <<- invmatrix
  
	getinverse <- function () inv

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve is used to calculate the inverse of a matrix stored in makeCacheMatrix
## If the the inverse was calculated earlier, then it returns the cached inverse matrix
## rather than calculating the inverse matrix value again. 

## The first input is always the makeCacheMatrix. Other paramters are optional parameters
## that can be used in Solve.

## Assumptions: 1. The matrix stored in makeCacheMatrix is inversible
## 2. Using different optional paramters would not change the cached output. (If you call cacheSolve(mycacheMat, op1) 
## and call cacheSolve(mycacheMat, op2) where op1 <> op2, would resturn same cache value). It is as per the home work spec.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached inverse matrix")
		return (inverse)
	}

	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse )
	inverse
        
}
