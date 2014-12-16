## The two functions below help with the matrix inversion.
##      makeCacheMatrix - This function creates a special matrix object that can cache it's inverse
##      cacheSolve - This function calculates the inverse of the matirx that was returned by the prevous function(makeCacheMatrix)
##
##
## This function retruns the functions to set, get, setinv and getinv of a special matrix that it created.
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
## set     
    set <- function(y) {

        x <<- y
        
	inverse <<- NULL
   
    }
##
##get 
    get <- function() x
##
## set inverse 
   setinv<- function(inverse_o) inverse <<-inverse_o

##
## get inverse 
	getinv <- function() inverse
list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The function cacheSolve returns the inverse of a matrix that was returned by the 
##  makeCacheMatrix function.

## If the cached inverse is available, cacheSolve retrieves it.
## If the cached inverse is not available, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {

	inverse = x$getinv()
		if (!is.null(inverse)){
                   return(inverse)
        	}else {
        
		     	inverse <- solve(x$get())
        
	             	x$setinv(inverse)
        
		     	return(inverse)
        
		}

}		
