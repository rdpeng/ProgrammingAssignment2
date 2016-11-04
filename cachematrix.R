## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
##
## This file contains two functions that allows caching a matrix and its inverse
##
## You can use solve() function  to calculate the inverse matrix but not works 
## for all matrices. For example, solve() generates an error with matrix
## matrix(c(1, 1, 2, 2), nrow = 2, ncol = 2).
##
## For that reason, I've decided to use the ginv() function included in MASS 
## package, that works for all matrices.


## Import MASS package in order to use ginv() function
library('MASS')


## Description:
## This function creates a special "matrix" object that can cache its inverse. 
## 
## Usage:
## makeCacheMatrix(x)
##
## Arguments
## x Matrix for which the inverse is required to cache

makeCacheMatrix <- function(x = matrix()){
    #Initialize the variable that will contain the inverse matrix
    inv <- NULL
    
    #This function storages the original matrix in the cache
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    #This function returns the original matrix from the cache
    get <- function(){ 
        x
    }
    
    #This function storage the inverse matrix in the cache
    setInv <- function(inverse){
        inv <<- inverse
    }    
    
    #This function returns the inverse matrix from the cache
    getInv <- function(){ 
        inv
    }
    
    #Returns a list of defined functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Description:
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 
## 
## Usage:
## cacheSolve(x, ...)
##
## Arguments
## x special "matrix" obtained from makeCacheMatrix function
## ... optional arguments to ginv function

cacheSolve <- function(x, ...){
    ## Get the inverse from the cache
    inv <- x$getInv()
    
    ## If the inverse has been previously calculated, get it from the cache
    if(!is.null(inv)){
        message('getting cached data')
        return(inv)
    }
    
    # Get the original matrix for which the inverse is required
    matrix <- x$get()
    
    # Calculates the inverse of the matrix
    inv <- ginv(matrix,...)
    
    # Stores the inverse in cache
    x$setInv(inv)
    
    # Returns the inverse matrix calculated by ginv function
    inv
}


