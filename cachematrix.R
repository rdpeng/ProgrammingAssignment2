##  This function creates a special "matrix" object that can cache its inverse
## SetInverse will set value of the invertible matrix and 
## GetInverse will bring value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
#set matrix value	
	set <- function(y){
		x <<- y
		inv <<- NULL 
	}
	get <- function ()x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse) 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       
 # Return a matrix that is the inverse of 'x'
 
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("Getting cached invertible matrix")
		return(inv)
	}
	Matrix <x$get()
	inv <- solve(Matrix)
	x$setInverse(inv)
	inv
}
