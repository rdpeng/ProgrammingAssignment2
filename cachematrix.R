## John Bello
## May 23, 2019

## The following functions will first create a matrix whose inverse will be 
## placed into a cache. This allows for faster computation in the event that the 
## matrix inversion will be required multiple times.

## makeCacheMatrix creates the matrix object that will be stored in the cache

makeCacheMatrix <- function(x = matrix()) {
        # create cached object 
        cached_mat <- NULL

        # allows matrix to be set, sets cached to NULL for conditional
        # in cacheSolve
        set <- function(new_mat) {
                x <<- new_mat
                cached_mat <<- NULL
        }

        # retrieves matrix
        get_mat <- function() x 
        
        # solves for inverse of matrix
        set_inverse <- function(inverted_mat) cached_mat <<- inverted_mat
        
        # retrieves cached matrix
        get_inverse <- function() cached_mat 

        list(set = set, get = get_mat,
             setInverse = set_inverse,
             getInverse = get_inverse)

}


## cacheSolve will solve for the inverse of input the matrix 
## In the event that the inverse has already been stored, the function 
## will instead retrieve the calculated inverse.   

cacheSolve <- function(x, ...) {
        # retrieves inverse 
        inverse_mat <- x$getInverse()
        
        # Checks if inverse has already been placed in cache
        if (!is.null(inverse_mat) & ){
                message('Retrieving inverted matrix')
                return(inverse_mat)
        }

        # retrieves matrix, solves for inverse, caches/returns solution
        matrix <- x$get_mat()
        inverted_mat <- solve(matrix, ...)
        x$setInverse(inverted_mat)
        inverted_mat
}
