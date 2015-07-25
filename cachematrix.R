## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  	##create matrix)
	s <- NULL 				##set s to NULL if it has not been calculated before
	set <- function(y) { 
		x <<- y 
		s <<- NULL
	} 
	get <- function() x 			##Notify that s has been calculated before
	setsolve <- function(solve) s <<- solve 
	getsolve <- function() s 
	list(set = set, get = get, 
		setsolve = setsolve, 	
		getsolve = getsolve) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {   	##Return of matrix that is the inverse of"x"
	s <- x$getsolve() 
	if(!is.null(s)) { 			##retrieve from cache if value hasn't changed
		message("getting cached data") 
		return(s) 
	} 
	data <- x$get()            		##value not in cache, calculate it
	s <- solve(data, ...) 
	x$setsolve(s) 
	i 

}
