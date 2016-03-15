# Programming Assignment 2: Lexical Scoping 


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
        get = get,
        setInverse = setInverse,
             getInverse = getInverse)
	}


The following function calculates the inverse of a "special" matrix created with   
# makeCacheMatrix  

cacheSolve <- function(y, ...) {  

# get the cached value  

	inverse <- y$getInverse()  

# if a cached value exists return it  

	if(!is.null(inverse)) {  
	message("getting cached data")  
	return(inverse)  
	}  

# otherwise get the matrix, caclulate the inverse and store it in  
# the cache  

	data <- y$getMatrix()  
	inverse <- solve(data)  
	y$cacheInverse(inverse)  
          
# return the inverse  

	inverse  
}  
