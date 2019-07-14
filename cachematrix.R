# Coursera Data Science Specialization (John Hopkins)
# Course 2 - R Programming
# Week 3
#
# Programming Project 2
# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a 
# matrix rather than computing it repeatedly (there are 
# also alternatives to matrix inversion that we will not 
# discuss here). Your assignment is to write a pair of 
# functions that cache the inverse of a matrix.
# 
# Write the following functions:
#     
# * makeCacheMatrix: This function creates a special 
#   "matrix" object that can cache its inverse.
# * cacheSolve: This function computes the inverse of the 
#   special "matrix" returned by makeCacheMatrix above. If 
#   the inverse has already been calculated (and the matrix 
#   has not changed), then cacheSolve should retrieve the 
#   inverse from the cache.
#   
# Computing the inverse of a square matrix can be done with 
# the solve function in R. For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is 
# always invertible.
#
# Mark Pedigo
# 7/14/2019

## Creates a special "matrix" object that can cache 
## its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

M = matrix(c(2, 4, 3, 1), nrow=2, ncol=2, byrow = TRUE)
cache <- makeCacheMatrix(M)
print(cacheSolve(cache))
