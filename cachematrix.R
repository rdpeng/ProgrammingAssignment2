# Learning R Second Week Assignment
# Kent Richardson
# March 15, 2016
#
###############################
#
# The second programming assignment will require you to write an R function able to cache 
# potentially time-consuming computations. For example, taking the mean of a numeric vector
# is typically a fast operation. However, for a very long vector, it may take too long to
# compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the
# contents of a vector are not changing, it may make sense to cache the value of the mean
# so that when we need it again, it can be looked up in the cache rather than recomputed. 
# In this Programming Assignment will take advantage of the scoping rules of the R language
# and how they can be manipulated to preserve state inside of an R object.
# 
# https://github.com/rdpeng/ProgrammingAssignment2

makeCacheMatrix <- function(xMatrix = matrix()) {
        # Creates a special matrix object that can cache its inverse
        #
        # The special matrix object will be a list of functions to:
        #     1. set/get the underlying matrix
        #     2. set/get the mean of the matrix
        #
        # Set up the functions supported by the special matrix
        mInverse <- NULL
        set <- function(y) {
                xMatrix <<- y #xMatrix holds the matrix data
                mInverse <<- NULL #yMtrix holds the inverted matrix data
        }
        get <- function() xMatrix
        setInverse <- function(i) mInverse <<- i
        getInverse <- function() mInverse
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

cacheSolve <- function(matrixM) {
        # This function computes the inverse of the special "matrix". The special "matrix" is returned by makeCacheMatrix
        #     above. If the inverse has already been calculated (and the matrix has not changed), then the
        #     cachesolve should retrieve the inverse from the cache.
        # Check if the inverse already exists for the special matrix
        mInverse <- matrixM$getInverse()
        if (!is.null(mInverse)) {
                message("Getting cached inverted matrix")
                return(mInverse)
        } else {
                baseMatrix <- matrixM$get()
                mInverse <- solve(baseMatrix)
                matrixM$setInverse(mInverse)
                return(mInverse)
        }
}
