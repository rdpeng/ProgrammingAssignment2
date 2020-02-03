## R Programming Assignment 2 Week 3
## Lexical Scoping
## Mary Maina

## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

## Write the following functions:

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse. For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }


cacheSolve <- function(x, ...) {
        ## Return the inverse of 'x' in a matrix form.
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
    }

## Testing my matrix.

 aMatrix <- makeCacheMatrix(matrix(c(-2, 4, 1, -3),2,2))
 
# aMatrix$set(matrix(c(-2, 4, 1, -3),2,2)) 
 aMatrix$get()
 aMatrix$getInverse()
 
## Testing the inverse of the matrix.
cacheSolve(aMatrix)
cacheSolve(aMatrix)
aMatrix$getInverse()
aMatrix$set(matrix(c(4, 4, 2, 8),2,2)) 
aMatrix$get()
cacheSolve(aMatrix)
