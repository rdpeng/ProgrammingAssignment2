## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.  
## This source file contains a pair of functions that cache the inverse of a matrix.
##
## Computing the inverse of a square matrix is done here using the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## This assignment, assume that the matrix supplied is always invertible.

## __________________________________________________________________________
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

}

## ___________________________________________________________________________
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
