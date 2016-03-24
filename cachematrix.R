## File:    cachematrix.R
## Date:    24-03-2016
## Author:  bartvdt
## Version: 1
##
## Contents: This file containts 2 functions:
## [1] makeCacheMatrix: to create a special matrix, a matrix with functions to facilitate
##     reuse of inverse matrix calculations
## [2] cacheSolve: creates inverse of a special matrix, using existing result if applicable
##
## Usage: If m is a matrix: cacheSolve( makeCacheMatrix (m) )

###############################################################################
## Function:     makeCacheMatrix
## Parameter(s): Singular matrix (Coursera assumption: matrix m is reversible)
##
## This function creates a special matrix that facilitates reuse of inverse
## matrix calculation. It provides 4 special matrix functions for this purpose:
## set (matrix), get (matrix), setinverse, getinverse
###############################################################################
makeCacheMatrix <- function(x) {
    ReverseMatrix <- NULL
    set <- function(y){
        x <<- y
        ReverseMatrix <- NULL
    }
    get <- function() x
    setinverse <- function(solve) ReverseMatrix <<- solve
    getinverse <- function() ReverseMatrix
    list( set = set
          ,get = get
          ,setinverse = setinverse
          ,getinverse = getinverse)
}

###############################################################################
## Function:     cacheSolve
## Parameter(s): Special matrix created by makeCacheMatrix
##
## This function returns the inverse of a special matrix
## First it checks whether inversion for this special matrix x has been calculated
## already. If this is true it obtains the inverse matrix from cache, otherwise
## it carries out the invers calculation. 
###############################################################################
cacheSolve <- function(x, ...) {
    ReverseMatrix <- x$getinverse()
    if (!is.null(ReverseMatrix)) {
        message("Return cached inverse matrix")
        return(ReverseMatrix)
    }
    message("Return calculation of inverse matrix")
    SourceMatrix <- x$get()
    ReverseMatrix <-solve(SourceMatrix, ...)
    x$setinverse(ReverseMatrix)
    ReverseMatrix
}