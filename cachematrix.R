## Taking the inverse of a matrix is usually quick for a singule matrix.
## However, if you need to repeatedly inverse the matrix can be time
## consuming over a large data set. Therefore, it might be of use to cache
## the inverse of a non-changing matrix and bring it up later on rather 
## than continually computing it.

## The first function, `makeCacheMatrix` creates a special "matrix" and allows
## the inverse of the matrix to be cached. The function...

## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse matrix
## 4. gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		
	inv <- NULL		##inv is inverse of matrix x
	set <- function(y) {
		x <<- y		## <<- assigns a value to an object in an
		inv <<- NULL	##environment that is different from the 
	}				##current environment
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(	set = set,
		get = get, 
		setinv = setinverse,
		getinv = getinverse)
}

## The following function calculates the inverse matrix of the special 
## "matrix" created with the above function. However, it first checks to
## see if the inverse matrix has already been calculated. If so, it 
## `get`s the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the mean in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {

	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
