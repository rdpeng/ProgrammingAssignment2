## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){ 
	m <- NULL  
	
	set <- function(y){  
		x <<- y    
		m <<- NULL  
	}  
	
	get <- function() x 
	
	setInverse <- function(solve) m<<- solve  
	
	getInverse <- function() m 
	
	list(set = set, get = get,  
		setInverse = setInverse,  
		getInverse = getInverse)  
}


## makeCacheMatrix : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("get cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
