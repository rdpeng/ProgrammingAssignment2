## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following functions cache and compute the inverse of a matrix

## "makeCacheMatrix" function creates a special "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inverse <- NULL
        s <- function(y) {
                x <<- y;
                inverse <<- NULL;
        }
        g <- function() return(x);
        
        sinv <- function(inv) inverse <<- inv;
        
        ginv <- function() return(inverse);
        
        return(list(s = s, g = g, sinv = sinv, ginv = ginv))
}


## Write a short comment describing this function

## "cacheSolve" function executes the inverse of the special
## "matrix" thats returned by `makeCacheMatrix` function. If the inverse has
## already been calculated and the matrix has not changed, then
## `cacheSolve` function should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$ginv()
        if(!is.null(inverse)) {
                
                message("Getting cached data...")
                
                return(inverse)
        }
        data <- x$g()
        
        invserse <- solve(data, ...)
        
        x$sinv(inverse)
        
        return(inverse)
}
