# Overall description: 
# These functions calculate the inverse of a matrix in a time saving manner.
# This is achieved by caching the matrix. 

# First function:
# this function makeCacheMatrix creates a special matrix, which is really a list with functions to

# I. set the value of the matrix
# II. get the value of the matrix
# III. set the inverse of the matrix
# IV. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function(x)
        set_inv_matrix <- function(inverse) m <<- inverse
        get_inv_matrix <- function() m
        list(set = set, get = get, set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}


# Second function:
# this function calculates the inverse of the matrix that has been created with makeCacheMatrix
# First, it checks whether the inverse has already been calculated. If that is true, then it skips computation.
# Secondly, it creates the inverse of the matrix and sets the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$get_inv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv_matrix(m)
        m
}
