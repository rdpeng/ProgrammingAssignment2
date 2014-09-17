#-----------------------------------
#  Program
#  Purpose:  To speed up the process of matrix inversion by calculating 
#            an invertable matrix and then caching a copy of it for subsequent
#            further use.
#
#  Usage:    Assuming the working directory is set properly
#            Example at the command line:
#            > source("matrixcache.R")
#            > m <- matrix(c(1,45,-23,55,-12,9,64,21,18), nrow=3,ncol=3)#            
#            > mat <- makeCacheMatrix(m)
#            > cacheSolve(mat)
#------------------------------------

#------------------------------------
# Function Purpose:
# 
# Arguements:    defined_matrix: a matrix
# Returns:       a function "wrapped" around a list of functions of an 
#                invertable matrix.
#                
# Functions:     1) set(x) - sets/caches a raw matrix where 'x' is the matrix.
#                2) get - returns the raw matrix.
#                3) setinverse
# Assumptions:   matrix is invertable as per the homework instructions
##------------------------------------
makeCacheMatrix <- function(defined_matrix = matrix()) {
    inverse_matrix <- NULL
    
    set <- function(y) {
        defined_matrix <<- y
        inverse_matrix <<- NULL
    }
    get <- function() defined_matrix
    setinverse <- function(new_inverse) inverse_matrix <<- new_inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#------------------------------
# Returns: an inverted matrix
#------------------------------
cacheSolve <- function(m, ...) {
    
    #call inverted matrix
    #if (!identical(m, ))
    inverse <- m$getinverse()
    
    #if inverted matrix exists
    #return it to caller
    if(!is.null(inverse)) {
        message("getting cached matrix")
        return(inverse)
    }
    
    #otherwise, get "raw" matrix so it can be inverted 
    # and cached for future use.    
    data <- m$get()
    
    #calculate inverse     
    inverse <- solve(data, ...)
    
    #cache the inverse and return it to calling function
    m$setinverse(inverse)
    inverse
}

