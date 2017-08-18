#Created By: Anshoo Mehra
#Course: Coursera Data Science, R Programming

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        if(!is.null(x)) {
                message("Matrix has been initialized...")
        } else {message("Error initializing Matrix, re-try or use makeCacheMatrix$set function...")}
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
                message("Matrix has been initialized...")
        }
        get <- function() x
        setinverse <- function(inverse) m_inv <<- inverse
        getinverse <- function() m_inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting Data From Cache...")
                return(inv)
        }
        message("Data Not found in Cache...")
        data <- x$get()
        message("Computing Inverse...")
        inv <- solve(data)
        message("Storing in Cache...")
        x$setinverse(inv)
        inv
}

# SAMPLE OUTPUT FRON TEST RUN ...
# > c <- matrix(1:4, 2, 2)
# > c
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# 
# > m = makeCacheMatrix(c)
# Matrix has been initialized...
# 
# > m = makeCacheMatrix(NULL)
# Error initializing Matrix, re-try or use makeCacheMatrix$set function...
# 
# > m = makeCacheMatrix(c)
# Matrix has been initialized...
# 
# > cacheSolve(m)
# Data Not found in Cache...
# Computing Inverse...
# Storing in Cache...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# 
# > cacheSolve(m)
# Getting Data From Cache...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
