## The functions are used to calculate the inverse of a matrix. In order to save computing time, 
##if the inverse of a matrix has been solved the inverse will be cached and solution can be get from cache memories 


makeCacheMatrix <- function(x = matrix()) {
		## makeCacheMatrix creates a special "matrix" object 
		## that can cache its inverse

		## it returns a list of functions which are
		## 1. set the value of the matrix
		## 2. get the value of the matrix
		## 3. set the value of the inverse
		## 4. get the value of the inverse

		s <- NULL
        	setmatrix <- function(y) {
                	x <<- y
                	s <<- NULL
        	}
        	getmatrix <- function() x
        	setsolve <- function(solve) s <<- solve
        	getsolve <- function() s
        	list(setmatrix = setmatrix, getmatrix = getmatrix,
             	setsolve = setsolve,
             	getsolve = getsolve)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                ## cacheSolve computes the inverse of the special "matrix"
		## returned by makeCacheMatrix above.
		
		## If the inverse has already been calculated
		## (and the matrix has not changed), 
		## then cacheSolve should retrieve the inverse from the cache.
		
		## Return a matrix that is the inverse of 'x'
		

		s <- x$getsolve()
       	if(!is.null(s)) {
                	message("getting cached data")
                	return(s)
        	}
        	data <- x$getmatrix()
        	s <- solve(data, ...)
        	x$setsolve(s)
        	s
        
}
