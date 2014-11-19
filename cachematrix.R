################################################################################
## Matrix inversion is usually a costly computation and there may be some 
##     benefit to caching the inverse of a matrix rather than computing it 
##     repeatedly.
## Here are a pair of functions that cache the inverse of a matrix.

################################################################################
## Set the working directory to the directory with the file program
## setwd("E:/Jorge/Curso/R/ProgrammingAssignment2")
## Sample run:
## > x = replicate(4, rnorm(4)) 
## > mtrx = makeCacheMatrix(x)
## > cacheSolve(mtrx)                ## Calculate the inverse
## > cacheSolve(mtrx)                ## Show the message "getting cached data"
## The result can be checked with this calculator
##      http://es.solvemymath.com/calculadoras/algebra/matriz/calculo_matriz.php


################################################################################
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initialite invMtrx value to null
        invMtrx <- NULL
        ## Funtion to set the value of the matrix
        set <- function(y) {
                x <<- y
                ## Initialite invMtrx value to null, because the value of
                ##      the matrix has changed
                invMtrx <<- NULL
        }
        ## Funtion to get the value of the matrix
        get <- function() x
        ## Funtion to set the value of the inverse of the matrix
        setinverse <- function(inverse) invMtrx <<- inverse
        ## Funtion to get the value of the inverse of the matrix
        getinverse <- function() invMtrx
        ## List of functions of the "matrix" object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

################################################################################
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Get the cached inverse matrix
        invMatrix <- x$getinverse()
        ## Checks to see if the inverse of the matrix has already calculated
        if(!is.null(invMatrix)) {
                ## Prompt a message
                message("getting cached data")
                ## Return the matrix that is the inverse of 'x' from cached data
                return(invMatrix)
        }
        ## Get the value of the matrix
        data <- x$get()
        ## Calculate the inverse of the matrix
        invMatrix <- solve(data, ...)
        ## Set the value of inverse of the matrix in cache
        x$setinverse(invMatrix)
        ## Return the matrix that is the inverse of 'x'
        return(invMatrix)
}
