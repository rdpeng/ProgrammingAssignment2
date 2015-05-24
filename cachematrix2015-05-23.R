## This source file contains a pair of functions
##      one of which, makeCacheMatrix handles caching the matrix and its inverse, and
##      the other of which, cacheSolve actually solves the inverse of the matrix

## Computing the inverse of a square matrix is done here using the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## This assignment, assume that the matrix supplied is always invertible.

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 

## __________________________________________________________________________
## This function creates an object that can cache a matrix and also cache its inverse
## in the Global Environ.
## The object has four properties: 
##          setMatrix,  used to store a matrix into the cache, and
##          setInverse, used to store the matrix's inverse into its own cache.
## and
##          getMatrix,  used to retrieve the matrix from its cache, nad 
##          getInverse, used to retrieve the inverse from its cache  
makeCacheMatrix <- function(parm_matX = matrix()) {
    g_Matrix <- parm_matX       ## local value
##    g_Inverse <- NULL
    setMatrix <- function(p_matrixY = matrix()) {
        g_Matrix <<- p_matrixY  ## y stored in Global Environ
        g_Inverse <<- NULL      ## g_Inverse is stored in Global Environ
    }
    getMatrix <- function() {
        g_Matrix           ## returns g_Matrix, the object from Global Environ
    }
    setInverse <- function(param_matX = matrix()) {
        g_Inverse <<- param_matX    ## param_matX, this function's parameter, is a matrix
    }
    getInverse <- function() {
        g_Inverse               ## returns g_Inverse, the inverse matrix, from Global Environ
    }
    list(setMatrix  = setMatrix, 
         getMatrix  = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}
## ___________________________________________________________________________
## This function computes the inverse of a matrix, p_x, and stores it in Global Environ
## using makeCacheMatrix above. If the inverse has already been calculated and stored
## in the Global Environ (and the matrix has not changed), then the cacheSolve retrieves 
## the inverse from the cache.
## x, the parameter is an object of class makeCacheMatrix()

cacheSolve <- function(x, ...) { 
    ## Returns a matrix that is the inverse of 'x' 
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}