#####################################################################################################
##  Function:           cachematrix.R
##  Author:		Dan Fraser
##  Created Date:	June 9, 2015
#####################################################################################################

##  makeCacheMatrix  is a function that stores a list of functions. These functions are set, get, 
##  setCacheMatrix and Get CacheMatrix. set is a function that changes the matrix stored in the 
##  main function. get is a function that returns the matrix x stored in the main function. 
##  setCacheMatrix stores the value of the input in a variable s1 into the main function and 
##  GetCacheMatrixc returns that value.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setCacheMatrix <- function(s1) s <<- s1
        getCacheMatrix <- function() s
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}


##  cachemean does is to verify that the value (s), stored previously with getCacheMatrix, exists 
##  and is not NULL. If it exists in memory, it simply returns a message and the value (s), that is 
##  supposed to be the inverse. If a value for s existed and was not NULL, "return(s)" would have 
##  ended the function. If s did not exist then data gets the matrix stored with makeCacheMatrix, (s) 
##  calculates the inverse of the matrix and x$setCacheMatrix(s) stores it in the object generated 
##  (s) assigned with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        s <- x$getCacheMatrix()
        if(!is.null(s)) {
                message("Getting cached data...")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setCacheMatrix(s)
        s
}
