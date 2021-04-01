## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		## get the value of the matrix
        get <- function() x
		## set the value of the inverse
        setinverse <- function(inverse) inv <<- inverse
        ##get the value of the inverse
		getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function

## This function computes the inverse of the matrix of the  function makeCacheMatrix by using solve(). 
##However, if the inverse has already been calculated, then the function cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'       
	   inv <- x$getinverse()
	   ##Checks  if the the inverse has already been calculated, and in such case, retrieves the inverse from the cache.
	   
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
		## otherwise computes the inverse of the matrix using solve().
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinverse(inv)
        inv
}
