## Put comments here that give an overall description of what your
## functions do

## Two functions provided that work together
## Function #1: makeCacheMatrix 
## Function #2: cacheSolved

## Send a 

################################
##
## FUNCTION: makeCacheMatrix
##
## Purpose:     Takes a matrix, creates inverse of matrix and caches the inverse.
## Arguments:   matrix to be inversed and cached
## Returns:     list with 4 functions to:
##              1. set the matrix
##              2. get the matrix
##              3. set inverted matrix
##              4. get the inverted matrix
##
################################
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        
        # set function: set matrix
        set <- function(y){
                x <<- y
                cache <<- NULL
        }
        
        # get function: get matrix that was sent to function
        get <- function() x
        
        #invert matrix
        setInverse <- function(inverse) cache <<- inverse
        
        #get inverted matrix
        getInverse <- function() cache
        
        #return functions to calling environment for use
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


################################
##
## FUNCTION:    cacheSolve
##
## Purpose:     Computes the inverse of 'special' matrix returned by function
##                      makeCacheMatrix. If the inverse is already calculated
##                      (and matrix hasn't changed), then cacheSolve retrieves
##                      the inverse from cache
## Arguments:   Matrix X (output from makeCacheMatrix), variable arguments 
## Returns:     Matrix that is the inverse of x
##
################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # use the getInverse() function returned by makeCacheMatrix
        #      to get the inverse matrix
        invMat <- x$getInverse()
        
        # check to see if inverse matrix exists, and return invMat
        if(!is.null(invMat)){
                message("returning cached data")
                return(invMat)
        }
        
        # doesn't exist so need get() from x, solve for the inverse and 
        #       setInverse()  based on functions returned
        #       by makeCacheMatrix       
        matrix <- x$get()
        
        # solve for inverse of matrix
        invMat <- solve(matrix, ...)
        
        # need to set the value of the inverse matrix created above
        x$setInverse(invMat)
        
        return(invMat)
}
